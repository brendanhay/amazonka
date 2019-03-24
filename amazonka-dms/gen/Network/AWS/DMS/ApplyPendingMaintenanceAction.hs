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
-- Module      : Network.AWS.DMS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a replication instance).
--
--
module Network.AWS.DMS.ApplyPendingMaintenanceAction
    (
    -- * Creating a Request
      applyPendingMaintenanceAction
    , ApplyPendingMaintenanceAction
    -- * Request Lenses
    , apmaReplicationInstanceARN
    , apmaApplyAction
    , apmaOptInType

    -- * Destructuring the Response
    , applyPendingMaintenanceActionResponse
    , ApplyPendingMaintenanceActionResponse
    -- * Response Lenses
    , apmarsResourcePendingMaintenanceActions
    , apmarsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'applyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { _apmaReplicationInstanceARN :: !Text
  , _apmaApplyAction            :: !Text
  , _apmaOptInType              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplyPendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apmaReplicationInstanceARN' - The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
--
-- * 'apmaApplyAction' - The pending maintenance action to apply to this resource.
--
-- * 'apmaOptInType' - A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ cannot be undone. Valid values:     * @immediate@ - Apply the maintenance action immediately.     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
applyPendingMaintenanceAction
    :: Text -- ^ 'apmaReplicationInstanceARN'
    -> Text -- ^ 'apmaApplyAction'
    -> Text -- ^ 'apmaOptInType'
    -> ApplyPendingMaintenanceAction
applyPendingMaintenanceAction pReplicationInstanceARN_ pApplyAction_ pOptInType_ =
  ApplyPendingMaintenanceAction'
    { _apmaReplicationInstanceARN = pReplicationInstanceARN_
    , _apmaApplyAction = pApplyAction_
    , _apmaOptInType = pOptInType_
    }


-- | The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
apmaReplicationInstanceARN :: Lens' ApplyPendingMaintenanceAction Text
apmaReplicationInstanceARN = lens _apmaReplicationInstanceARN (\ s a -> s{_apmaReplicationInstanceARN = a})

-- | The pending maintenance action to apply to this resource.
apmaApplyAction :: Lens' ApplyPendingMaintenanceAction Text
apmaApplyAction = lens _apmaApplyAction (\ s a -> s{_apmaApplyAction = a})

-- | A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ cannot be undone. Valid values:     * @immediate@ - Apply the maintenance action immediately.     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
apmaOptInType :: Lens' ApplyPendingMaintenanceAction Text
apmaOptInType = lens _apmaOptInType (\ s a -> s{_apmaOptInType = a})

instance AWSRequest ApplyPendingMaintenanceAction
         where
        type Rs ApplyPendingMaintenanceAction =
             ApplyPendingMaintenanceActionResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ApplyPendingMaintenanceActionResponse' <$>
                   (x .?> "ResourcePendingMaintenanceActions") <*>
                     (pure (fromEnum s)))

instance Hashable ApplyPendingMaintenanceAction where

instance NFData ApplyPendingMaintenanceAction where

instance ToHeaders ApplyPendingMaintenanceAction
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ApplyPendingMaintenanceAction"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ApplyPendingMaintenanceAction where
        toJSON ApplyPendingMaintenanceAction'{..}
          = object
              (catMaybes
                 [Just
                    ("ReplicationInstanceArn" .=
                       _apmaReplicationInstanceARN),
                  Just ("ApplyAction" .= _apmaApplyAction),
                  Just ("OptInType" .= _apmaOptInType)])

instance ToPath ApplyPendingMaintenanceAction where
        toPath = const "/"

instance ToQuery ApplyPendingMaintenanceAction where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'applyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { _apmarsResourcePendingMaintenanceActions :: !(Maybe ResourcePendingMaintenanceActions)
  , _apmarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplyPendingMaintenanceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apmarsResourcePendingMaintenanceActions' - The AWS DMS resource that the pending maintenance action will be applied to.
--
-- * 'apmarsResponseStatus' - -- | The response status code.
applyPendingMaintenanceActionResponse
    :: Int -- ^ 'apmarsResponseStatus'
    -> ApplyPendingMaintenanceActionResponse
applyPendingMaintenanceActionResponse pResponseStatus_ =
  ApplyPendingMaintenanceActionResponse'
    { _apmarsResourcePendingMaintenanceActions = Nothing
    , _apmarsResponseStatus = pResponseStatus_
    }


-- | The AWS DMS resource that the pending maintenance action will be applied to.
apmarsResourcePendingMaintenanceActions :: Lens' ApplyPendingMaintenanceActionResponse (Maybe ResourcePendingMaintenanceActions)
apmarsResourcePendingMaintenanceActions = lens _apmarsResourcePendingMaintenanceActions (\ s a -> s{_apmarsResourcePendingMaintenanceActions = a})

-- | -- | The response status code.
apmarsResponseStatus :: Lens' ApplyPendingMaintenanceActionResponse Int
apmarsResponseStatus = lens _apmarsResponseStatus (\ s a -> s{_apmarsResponseStatus = a})

instance NFData ApplyPendingMaintenanceActionResponse
         where
