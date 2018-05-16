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
-- Module      : Network.AWS.CloudFormation.StopStackSetOperation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an in-progress operation on a stack set and its associated stack instances.
--
--
module Network.AWS.CloudFormation.StopStackSetOperation
    (
    -- * Creating a Request
      stopStackSetOperation
    , StopStackSetOperation
    -- * Request Lenses
    , sssoStackSetName
    , sssoOperationId

    -- * Destructuring the Response
    , stopStackSetOperationResponse
    , StopStackSetOperationResponse
    -- * Response Lenses
    , sssorsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopStackSetOperation' smart constructor.
data StopStackSetOperation = StopStackSetOperation'
  { _sssoStackSetName :: !Text
  , _sssoOperationId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStackSetOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssoStackSetName' - The name or unique ID of the stack set that you want to stop the operation for.
--
-- * 'sssoOperationId' - The ID of the stack operation.
stopStackSetOperation
    :: Text -- ^ 'sssoStackSetName'
    -> Text -- ^ 'sssoOperationId'
    -> StopStackSetOperation
stopStackSetOperation pStackSetName_ pOperationId_ =
  StopStackSetOperation'
    {_sssoStackSetName = pStackSetName_, _sssoOperationId = pOperationId_}


-- | The name or unique ID of the stack set that you want to stop the operation for.
sssoStackSetName :: Lens' StopStackSetOperation Text
sssoStackSetName = lens _sssoStackSetName (\ s a -> s{_sssoStackSetName = a})

-- | The ID of the stack operation.
sssoOperationId :: Lens' StopStackSetOperation Text
sssoOperationId = lens _sssoOperationId (\ s a -> s{_sssoOperationId = a})

instance AWSRequest StopStackSetOperation where
        type Rs StopStackSetOperation =
             StopStackSetOperationResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "StopStackSetOperationResult"
              (\ s h x ->
                 StopStackSetOperationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StopStackSetOperation where

instance NFData StopStackSetOperation where

instance ToHeaders StopStackSetOperation where
        toHeaders = const mempty

instance ToPath StopStackSetOperation where
        toPath = const "/"

instance ToQuery StopStackSetOperation where
        toQuery StopStackSetOperation'{..}
          = mconcat
              ["Action" =: ("StopStackSetOperation" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackSetName" =: _sssoStackSetName,
               "OperationId" =: _sssoOperationId]

-- | /See:/ 'stopStackSetOperationResponse' smart constructor.
newtype StopStackSetOperationResponse = StopStackSetOperationResponse'
  { _sssorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStackSetOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssorsResponseStatus' - -- | The response status code.
stopStackSetOperationResponse
    :: Int -- ^ 'sssorsResponseStatus'
    -> StopStackSetOperationResponse
stopStackSetOperationResponse pResponseStatus_ =
  StopStackSetOperationResponse' {_sssorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sssorsResponseStatus :: Lens' StopStackSetOperationResponse Int
sssorsResponseStatus = lens _sssorsResponseStatus (\ s a -> s{_sssorsResponseStatus = a})

instance NFData StopStackSetOperationResponse where
