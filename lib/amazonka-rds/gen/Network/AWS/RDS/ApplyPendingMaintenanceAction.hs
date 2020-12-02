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
-- Module      : Network.AWS.RDS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a DB instance).
--
--
module Network.AWS.RDS.ApplyPendingMaintenanceAction
    (
    -- * Creating a Request
      applyPendingMaintenanceAction
    , ApplyPendingMaintenanceAction
    -- * Request Lenses
    , apmaResourceIdentifier
    , apmaApplyAction
    , apmaOptInType

    -- * Destructuring the Response
    , applyPendingMaintenanceActionResponse
    , ApplyPendingMaintenanceActionResponse
    -- * Response Lenses
    , apmarsResourcePendingMaintenanceActions
    , apmarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'applyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { _apmaResourceIdentifier :: !Text
  , _apmaApplyAction        :: !Text
  , _apmaOptInType          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplyPendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apmaResourceIdentifier' - The RDS Amazon Resource Name (ARN) of the resource that the pending maintenance action applies to. For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
--
-- * 'apmaApplyAction' - The pending maintenance action to apply to this resource. Valid values: @system-update@ , @db-upgrade@
--
-- * 'apmaOptInType' - A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ can't be undone. Valid values:     * @immediate@ - Apply the maintenance action immediately.     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
applyPendingMaintenanceAction
    :: Text -- ^ 'apmaResourceIdentifier'
    -> Text -- ^ 'apmaApplyAction'
    -> Text -- ^ 'apmaOptInType'
    -> ApplyPendingMaintenanceAction
applyPendingMaintenanceAction pResourceIdentifier_ pApplyAction_ pOptInType_ =
  ApplyPendingMaintenanceAction'
    { _apmaResourceIdentifier = pResourceIdentifier_
    , _apmaApplyAction = pApplyAction_
    , _apmaOptInType = pOptInType_
    }


-- | The RDS Amazon Resource Name (ARN) of the resource that the pending maintenance action applies to. For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
apmaResourceIdentifier :: Lens' ApplyPendingMaintenanceAction Text
apmaResourceIdentifier = lens _apmaResourceIdentifier (\ s a -> s{_apmaResourceIdentifier = a})

-- | The pending maintenance action to apply to this resource. Valid values: @system-update@ , @db-upgrade@
apmaApplyAction :: Lens' ApplyPendingMaintenanceAction Text
apmaApplyAction = lens _apmaApplyAction (\ s a -> s{_apmaApplyAction = a})

-- | A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ can't be undone. Valid values:     * @immediate@ - Apply the maintenance action immediately.     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
apmaOptInType :: Lens' ApplyPendingMaintenanceAction Text
apmaOptInType = lens _apmaOptInType (\ s a -> s{_apmaOptInType = a})

instance AWSRequest ApplyPendingMaintenanceAction
         where
        type Rs ApplyPendingMaintenanceAction =
             ApplyPendingMaintenanceActionResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "ApplyPendingMaintenanceActionResult"
              (\ s h x ->
                 ApplyPendingMaintenanceActionResponse' <$>
                   (x .@? "ResourcePendingMaintenanceActions") <*>
                     (pure (fromEnum s)))

instance Hashable ApplyPendingMaintenanceAction where

instance NFData ApplyPendingMaintenanceAction where

instance ToHeaders ApplyPendingMaintenanceAction
         where
        toHeaders = const mempty

instance ToPath ApplyPendingMaintenanceAction where
        toPath = const "/"

instance ToQuery ApplyPendingMaintenanceAction where
        toQuery ApplyPendingMaintenanceAction'{..}
          = mconcat
              ["Action" =:
                 ("ApplyPendingMaintenanceAction" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ResourceIdentifier" =: _apmaResourceIdentifier,
               "ApplyAction" =: _apmaApplyAction,
               "OptInType" =: _apmaOptInType]

-- | /See:/ 'applyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { _apmarsResourcePendingMaintenanceActions :: !(Maybe ResourcePendingMaintenanceActions)
  , _apmarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplyPendingMaintenanceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apmarsResourcePendingMaintenanceActions' - Undocumented member.
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


-- | Undocumented member.
apmarsResourcePendingMaintenanceActions :: Lens' ApplyPendingMaintenanceActionResponse (Maybe ResourcePendingMaintenanceActions)
apmarsResourcePendingMaintenanceActions = lens _apmarsResourcePendingMaintenanceActions (\ s a -> s{_apmarsResourcePendingMaintenanceActions = a})

-- | -- | The response status code.
apmarsResponseStatus :: Lens' ApplyPendingMaintenanceActionResponse Int
apmarsResponseStatus = lens _apmarsResponseStatus (\ s a -> s{_apmarsResponseStatus = a})

instance NFData ApplyPendingMaintenanceActionResponse
         where
