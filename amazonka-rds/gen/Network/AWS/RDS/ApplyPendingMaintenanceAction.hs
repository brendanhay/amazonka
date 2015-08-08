{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a DB
-- instance).
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ApplyPendingMaintenanceAction.html AWS API Reference> for ApplyPendingMaintenanceAction.
module Network.AWS.RDS.ApplyPendingMaintenanceAction
    (
    -- * Creating a Request
      ApplyPendingMaintenanceAction
    , applyPendingMaintenanceAction
    -- * Request Lenses
    , apmaResourceIdentifier
    , apmaApplyAction
    , apmaOptInType

    -- * Destructuring the Response
    , ApplyPendingMaintenanceActionResponse
    , applyPendingMaintenanceActionResponse
    -- * Response Lenses
    , apmarsResourcePendingMaintenanceActions
    , apmarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'applyPendingMaintenanceAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apmaResourceIdentifier'
--
-- * 'apmaApplyAction'
--
-- * 'apmaOptInType'
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
    { _apmaResourceIdentifier :: !Text
    , _apmaApplyAction        :: !Text
    , _apmaOptInType          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplyPendingMaintenanceAction' smart constructor.
applyPendingMaintenanceAction :: Text -> Text -> Text -> ApplyPendingMaintenanceAction
applyPendingMaintenanceAction pResourceIdentifier_ pApplyAction_ pOptInType_ =
    ApplyPendingMaintenanceAction'
    { _apmaResourceIdentifier = pResourceIdentifier_
    , _apmaApplyAction = pApplyAction_
    , _apmaOptInType = pOptInType_
    }

-- | The RDS Amazon Resource Name (ARN) of the resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing an RDS Amazon Resource Name (ARN)>.
apmaResourceIdentifier :: Lens' ApplyPendingMaintenanceAction Text
apmaResourceIdentifier = lens _apmaResourceIdentifier (\ s a -> s{_apmaResourceIdentifier = a});

-- | The pending maintenance action to apply to this resource.
apmaApplyAction :: Lens' ApplyPendingMaintenanceAction Text
apmaApplyAction = lens _apmaApplyAction (\ s a -> s{_apmaApplyAction = a});

-- | A value that specifies the type of opt-in request, or undoes an opt-in
-- request. An opt-in request of type @immediate@ cannot be undone.
--
-- Valid values:
--
-- -   @immediate@ - Apply the maintenance action immediately.
-- -   @next-maintenance@ - Apply the maintenance action during the next
--     maintenance window for the resource.
-- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
--     requests.
apmaOptInType :: Lens' ApplyPendingMaintenanceAction Text
apmaOptInType = lens _apmaOptInType (\ s a -> s{_apmaOptInType = a});

instance AWSRequest ApplyPendingMaintenanceAction
         where
        type Sv ApplyPendingMaintenanceAction = RDS
        type Rs ApplyPendingMaintenanceAction =
             ApplyPendingMaintenanceActionResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "ApplyPendingMaintenanceActionResult"
              (\ s h x ->
                 ApplyPendingMaintenanceActionResponse' <$>
                   (x .@? "ResourcePendingMaintenanceActions") <*>
                     (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apmarsResourcePendingMaintenanceActions'
--
-- * 'apmarsStatus'
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
    { _apmarsResourcePendingMaintenanceActions :: !(Maybe ResourcePendingMaintenanceActions)
    , _apmarsStatus                            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplyPendingMaintenanceActionResponse' smart constructor.
applyPendingMaintenanceActionResponse :: Int -> ApplyPendingMaintenanceActionResponse
applyPendingMaintenanceActionResponse pStatus_ =
    ApplyPendingMaintenanceActionResponse'
    { _apmarsResourcePendingMaintenanceActions = Nothing
    , _apmarsStatus = pStatus_
    }

-- | Undocumented member.
apmarsResourcePendingMaintenanceActions :: Lens' ApplyPendingMaintenanceActionResponse (Maybe ResourcePendingMaintenanceActions)
apmarsResourcePendingMaintenanceActions = lens _apmarsResourcePendingMaintenanceActions (\ s a -> s{_apmarsResourcePendingMaintenanceActions = a});

-- | Undocumented member.
apmarsStatus :: Lens' ApplyPendingMaintenanceActionResponse Int
apmarsStatus = lens _apmarsStatus (\ s a -> s{_apmarsStatus = a});
