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
-- Module      : Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a target with a Maintenance Window.
--
--
module Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
    (
    -- * Creating a Request
      registerTargetWithMaintenanceWindow
    , RegisterTargetWithMaintenanceWindow
    -- * Request Lenses
    , rClientToken
    , rOwnerInformation
    , rName
    , rDescription
    , rWindowId
    , rResourceType
    , rTargets

    -- * Destructuring the Response
    , registerTargetWithMaintenanceWindowResponse
    , RegisterTargetWithMaintenanceWindowResponse
    -- * Response Lenses
    , rrsWindowTargetId
    , rrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'registerTargetWithMaintenanceWindow' smart constructor.
data RegisterTargetWithMaintenanceWindow = RegisterTargetWithMaintenanceWindow'
  { _rClientToken      :: !(Maybe Text)
  , _rOwnerInformation :: !(Maybe (Sensitive Text))
  , _rName             :: !(Maybe Text)
  , _rDescription      :: !(Maybe (Sensitive Text))
  , _rWindowId         :: !Text
  , _rResourceType     :: !MaintenanceWindowResourceType
  , _rTargets          :: ![Target]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTargetWithMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rClientToken' - User-provided idempotency token.
--
-- * 'rOwnerInformation' - User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
--
-- * 'rName' - An optional name for the target.
--
-- * 'rDescription' - An optional description for the target.
--
-- * 'rWindowId' - The ID of the Maintenance Window the target should be registered with.
--
-- * 'rResourceType' - The type of target being registered with the Maintenance Window.
--
-- * 'rTargets' - The targets (either instances or tags).  Specify instances using the following format: @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@  Specify tags using either of the following formats: @Key=tag:<tag-key>,Values=<tag-value-1>,<tag-value-2>@  @Key=tag-key,Values=<tag-key-1>,<tag-key-2>@
registerTargetWithMaintenanceWindow
    :: Text -- ^ 'rWindowId'
    -> MaintenanceWindowResourceType -- ^ 'rResourceType'
    -> RegisterTargetWithMaintenanceWindow
registerTargetWithMaintenanceWindow pWindowId_ pResourceType_ =
  RegisterTargetWithMaintenanceWindow'
    { _rClientToken = Nothing
    , _rOwnerInformation = Nothing
    , _rName = Nothing
    , _rDescription = Nothing
    , _rWindowId = pWindowId_
    , _rResourceType = pResourceType_
    , _rTargets = mempty
    }


-- | User-provided idempotency token.
rClientToken :: Lens' RegisterTargetWithMaintenanceWindow (Maybe Text)
rClientToken = lens _rClientToken (\ s a -> s{_rClientToken = a})

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
rOwnerInformation :: Lens' RegisterTargetWithMaintenanceWindow (Maybe Text)
rOwnerInformation = lens _rOwnerInformation (\ s a -> s{_rOwnerInformation = a}) . mapping _Sensitive

-- | An optional name for the target.
rName :: Lens' RegisterTargetWithMaintenanceWindow (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | An optional description for the target.
rDescription :: Lens' RegisterTargetWithMaintenanceWindow (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a}) . mapping _Sensitive

-- | The ID of the Maintenance Window the target should be registered with.
rWindowId :: Lens' RegisterTargetWithMaintenanceWindow Text
rWindowId = lens _rWindowId (\ s a -> s{_rWindowId = a})

-- | The type of target being registered with the Maintenance Window.
rResourceType :: Lens' RegisterTargetWithMaintenanceWindow MaintenanceWindowResourceType
rResourceType = lens _rResourceType (\ s a -> s{_rResourceType = a})

-- | The targets (either instances or tags).  Specify instances using the following format: @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@  Specify tags using either of the following formats: @Key=tag:<tag-key>,Values=<tag-value-1>,<tag-value-2>@  @Key=tag-key,Values=<tag-key-1>,<tag-key-2>@
rTargets :: Lens' RegisterTargetWithMaintenanceWindow [Target]
rTargets = lens _rTargets (\ s a -> s{_rTargets = a}) . _Coerce

instance AWSRequest
           RegisterTargetWithMaintenanceWindow
         where
        type Rs RegisterTargetWithMaintenanceWindow =
             RegisterTargetWithMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 RegisterTargetWithMaintenanceWindowResponse' <$>
                   (x .?> "WindowTargetId") <*> (pure (fromEnum s)))

instance Hashable RegisterTargetWithMaintenanceWindow
         where

instance NFData RegisterTargetWithMaintenanceWindow
         where

instance ToHeaders
           RegisterTargetWithMaintenanceWindow
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.RegisterTargetWithMaintenanceWindow" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterTargetWithMaintenanceWindow
         where
        toJSON RegisterTargetWithMaintenanceWindow'{..}
          = object
              (catMaybes
                 [("ClientToken" .=) <$> _rClientToken,
                  ("OwnerInformation" .=) <$> _rOwnerInformation,
                  ("Name" .=) <$> _rName,
                  ("Description" .=) <$> _rDescription,
                  Just ("WindowId" .= _rWindowId),
                  Just ("ResourceType" .= _rResourceType),
                  Just ("Targets" .= _rTargets)])

instance ToPath RegisterTargetWithMaintenanceWindow
         where
        toPath = const "/"

instance ToQuery RegisterTargetWithMaintenanceWindow
         where
        toQuery = const mempty

-- | /See:/ 'registerTargetWithMaintenanceWindowResponse' smart constructor.
data RegisterTargetWithMaintenanceWindowResponse = RegisterTargetWithMaintenanceWindowResponse'
  { _rrsWindowTargetId :: !(Maybe Text)
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTargetWithMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsWindowTargetId' - The ID of the target definition in this Maintenance Window.
--
-- * 'rrsResponseStatus' - -- | The response status code.
registerTargetWithMaintenanceWindowResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> RegisterTargetWithMaintenanceWindowResponse
registerTargetWithMaintenanceWindowResponse pResponseStatus_ =
  RegisterTargetWithMaintenanceWindowResponse'
    {_rrsWindowTargetId = Nothing, _rrsResponseStatus = pResponseStatus_}


-- | The ID of the target definition in this Maintenance Window.
rrsWindowTargetId :: Lens' RegisterTargetWithMaintenanceWindowResponse (Maybe Text)
rrsWindowTargetId = lens _rrsWindowTargetId (\ s a -> s{_rrsWindowTargetId = a})

-- | -- | The response status code.
rrsResponseStatus :: Lens' RegisterTargetWithMaintenanceWindowResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData
           RegisterTargetWithMaintenanceWindowResponse
         where
