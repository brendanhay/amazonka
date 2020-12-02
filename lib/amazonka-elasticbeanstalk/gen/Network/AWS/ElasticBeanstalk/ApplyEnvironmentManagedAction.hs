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
-- Module      : Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a scheduled managed action immediately. A managed action can be applied only if its status is @Scheduled@ . Get the status and action ID of a managed action with 'DescribeEnvironmentManagedActions' .
--
--
module Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
    (
    -- * Creating a Request
      applyEnvironmentManagedAction
    , ApplyEnvironmentManagedAction
    -- * Request Lenses
    , aemaEnvironmentName
    , aemaEnvironmentId
    , aemaActionId

    -- * Destructuring the Response
    , applyEnvironmentManagedActionResponse
    , ApplyEnvironmentManagedActionResponse
    -- * Response Lenses
    , aemarsStatus
    , aemarsActionId
    , aemarsActionDescription
    , aemarsActionType
    , aemarsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to execute a scheduled managed action immediately.
--
--
--
-- /See:/ 'applyEnvironmentManagedAction' smart constructor.
data ApplyEnvironmentManagedAction = ApplyEnvironmentManagedAction'
  { _aemaEnvironmentName :: !(Maybe Text)
  , _aemaEnvironmentId   :: !(Maybe Text)
  , _aemaActionId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplyEnvironmentManagedAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aemaEnvironmentName' - The name of the target environment.
--
-- * 'aemaEnvironmentId' - The environment ID of the target environment.
--
-- * 'aemaActionId' - The action ID of the scheduled managed action to execute.
applyEnvironmentManagedAction
    :: Text -- ^ 'aemaActionId'
    -> ApplyEnvironmentManagedAction
applyEnvironmentManagedAction pActionId_ =
  ApplyEnvironmentManagedAction'
    { _aemaEnvironmentName = Nothing
    , _aemaEnvironmentId = Nothing
    , _aemaActionId = pActionId_
    }


-- | The name of the target environment.
aemaEnvironmentName :: Lens' ApplyEnvironmentManagedAction (Maybe Text)
aemaEnvironmentName = lens _aemaEnvironmentName (\ s a -> s{_aemaEnvironmentName = a})

-- | The environment ID of the target environment.
aemaEnvironmentId :: Lens' ApplyEnvironmentManagedAction (Maybe Text)
aemaEnvironmentId = lens _aemaEnvironmentId (\ s a -> s{_aemaEnvironmentId = a})

-- | The action ID of the scheduled managed action to execute.
aemaActionId :: Lens' ApplyEnvironmentManagedAction Text
aemaActionId = lens _aemaActionId (\ s a -> s{_aemaActionId = a})

instance AWSRequest ApplyEnvironmentManagedAction
         where
        type Rs ApplyEnvironmentManagedAction =
             ApplyEnvironmentManagedActionResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "ApplyEnvironmentManagedActionResult"
              (\ s h x ->
                 ApplyEnvironmentManagedActionResponse' <$>
                   (x .@? "Status") <*> (x .@? "ActionId") <*>
                     (x .@? "ActionDescription")
                     <*> (x .@? "ActionType")
                     <*> (pure (fromEnum s)))

instance Hashable ApplyEnvironmentManagedAction where

instance NFData ApplyEnvironmentManagedAction where

instance ToHeaders ApplyEnvironmentManagedAction
         where
        toHeaders = const mempty

instance ToPath ApplyEnvironmentManagedAction where
        toPath = const "/"

instance ToQuery ApplyEnvironmentManagedAction where
        toQuery ApplyEnvironmentManagedAction'{..}
          = mconcat
              ["Action" =:
                 ("ApplyEnvironmentManagedAction" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentName" =: _aemaEnvironmentName,
               "EnvironmentId" =: _aemaEnvironmentId,
               "ActionId" =: _aemaActionId]

-- | The result message containing information about the managed action.
--
--
--
-- /See:/ 'applyEnvironmentManagedActionResponse' smart constructor.
data ApplyEnvironmentManagedActionResponse = ApplyEnvironmentManagedActionResponse'
  { _aemarsStatus            :: !(Maybe Text)
  , _aemarsActionId          :: !(Maybe Text)
  , _aemarsActionDescription :: !(Maybe Text)
  , _aemarsActionType        :: !(Maybe ActionType)
  , _aemarsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplyEnvironmentManagedActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aemarsStatus' - The status of the managed action.
--
-- * 'aemarsActionId' - The action ID of the managed action.
--
-- * 'aemarsActionDescription' - A description of the managed action.
--
-- * 'aemarsActionType' - The type of managed action.
--
-- * 'aemarsResponseStatus' - -- | The response status code.
applyEnvironmentManagedActionResponse
    :: Int -- ^ 'aemarsResponseStatus'
    -> ApplyEnvironmentManagedActionResponse
applyEnvironmentManagedActionResponse pResponseStatus_ =
  ApplyEnvironmentManagedActionResponse'
    { _aemarsStatus = Nothing
    , _aemarsActionId = Nothing
    , _aemarsActionDescription = Nothing
    , _aemarsActionType = Nothing
    , _aemarsResponseStatus = pResponseStatus_
    }


-- | The status of the managed action.
aemarsStatus :: Lens' ApplyEnvironmentManagedActionResponse (Maybe Text)
aemarsStatus = lens _aemarsStatus (\ s a -> s{_aemarsStatus = a})

-- | The action ID of the managed action.
aemarsActionId :: Lens' ApplyEnvironmentManagedActionResponse (Maybe Text)
aemarsActionId = lens _aemarsActionId (\ s a -> s{_aemarsActionId = a})

-- | A description of the managed action.
aemarsActionDescription :: Lens' ApplyEnvironmentManagedActionResponse (Maybe Text)
aemarsActionDescription = lens _aemarsActionDescription (\ s a -> s{_aemarsActionDescription = a})

-- | The type of managed action.
aemarsActionType :: Lens' ApplyEnvironmentManagedActionResponse (Maybe ActionType)
aemarsActionType = lens _aemarsActionType (\ s a -> s{_aemarsActionType = a})

-- | -- | The response status code.
aemarsResponseStatus :: Lens' ApplyEnvironmentManagedActionResponse Int
aemarsResponseStatus = lens _aemarsResponseStatus (\ s a -> s{_aemarsResponseStatus = a})

instance NFData ApplyEnvironmentManagedActionResponse
         where
