{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartRemediationExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand remediation for the specified AWS Config rules against the last known remediation configuration. It runs an execution against the current state of your resources. Remediation execution is asynchronous.
--
--
-- You can specify up to 100 resource keys per request. An existing StartRemediationExecution call for the specified resource keys must complete before you can call the API again.
--
module Network.AWS.Config.StartRemediationExecution
    (
    -- * Creating a Request
      startRemediationExecution
    , StartRemediationExecution
    -- * Request Lenses
    , sreConfigRuleName
    , sreResourceKeys

    -- * Destructuring the Response
    , startRemediationExecutionResponse
    , StartRemediationExecutionResponse
    -- * Response Lenses
    , srersFailureMessage
    , srersFailedItems
    , srersResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startRemediationExecution' smart constructor.
data StartRemediationExecution = StartRemediationExecution'
  { _sreConfigRuleName :: !Text
  , _sreResourceKeys   :: !(List1 ResourceKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartRemediationExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sreConfigRuleName' - The list of names of AWS Config rules that you want to run remediation execution for.
--
-- * 'sreResourceKeys' - A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
startRemediationExecution
    :: Text -- ^ 'sreConfigRuleName'
    -> NonEmpty ResourceKey -- ^ 'sreResourceKeys'
    -> StartRemediationExecution
startRemediationExecution pConfigRuleName_ pResourceKeys_ =
  StartRemediationExecution'
    { _sreConfigRuleName = pConfigRuleName_
    , _sreResourceKeys = _List1 # pResourceKeys_
    }


-- | The list of names of AWS Config rules that you want to run remediation execution for.
sreConfigRuleName :: Lens' StartRemediationExecution Text
sreConfigRuleName = lens _sreConfigRuleName (\ s a -> s{_sreConfigRuleName = a})

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
sreResourceKeys :: Lens' StartRemediationExecution (NonEmpty ResourceKey)
sreResourceKeys = lens _sreResourceKeys (\ s a -> s{_sreResourceKeys = a}) . _List1

instance AWSRequest StartRemediationExecution where
        type Rs StartRemediationExecution =
             StartRemediationExecutionResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 StartRemediationExecutionResponse' <$>
                   (x .?> "FailureMessage") <*> (x .?> "FailedItems")
                     <*> (pure (fromEnum s)))

instance Hashable StartRemediationExecution where

instance NFData StartRemediationExecution where

instance ToHeaders StartRemediationExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.StartRemediationExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartRemediationExecution where
        toJSON StartRemediationExecution'{..}
          = object
              (catMaybes
                 [Just ("ConfigRuleName" .= _sreConfigRuleName),
                  Just ("ResourceKeys" .= _sreResourceKeys)])

instance ToPath StartRemediationExecution where
        toPath = const "/"

instance ToQuery StartRemediationExecution where
        toQuery = const mempty

-- | /See:/ 'startRemediationExecutionResponse' smart constructor.
data StartRemediationExecutionResponse = StartRemediationExecutionResponse'
  { _srersFailureMessage :: !(Maybe Text)
  , _srersFailedItems    :: !(Maybe (List1 ResourceKey))
  , _srersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartRemediationExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srersFailureMessage' - Returns a failure message. For example, the resource is already compliant.
--
-- * 'srersFailedItems' - For resources that have failed to start execution, the API returns a resource key object.
--
-- * 'srersResponseStatus' - -- | The response status code.
startRemediationExecutionResponse
    :: Int -- ^ 'srersResponseStatus'
    -> StartRemediationExecutionResponse
startRemediationExecutionResponse pResponseStatus_ =
  StartRemediationExecutionResponse'
    { _srersFailureMessage = Nothing
    , _srersFailedItems = Nothing
    , _srersResponseStatus = pResponseStatus_
    }


-- | Returns a failure message. For example, the resource is already compliant.
srersFailureMessage :: Lens' StartRemediationExecutionResponse (Maybe Text)
srersFailureMessage = lens _srersFailureMessage (\ s a -> s{_srersFailureMessage = a})

-- | For resources that have failed to start execution, the API returns a resource key object.
srersFailedItems :: Lens' StartRemediationExecutionResponse (Maybe (NonEmpty ResourceKey))
srersFailedItems = lens _srersFailedItems (\ s a -> s{_srersFailedItems = a}) . mapping _List1

-- | -- | The response status code.
srersResponseStatus :: Lens' StartRemediationExecutionResponse Int
srersResponseStatus = lens _srersResponseStatus (\ s a -> s{_srersResponseStatus = a})

instance NFData StartRemediationExecutionResponse
         where
