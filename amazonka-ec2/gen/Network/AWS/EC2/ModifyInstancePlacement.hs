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
-- Module      : Network.AWS.EC2.ModifyInstancePlacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the placement attributes for a specified instance. You can do the following:
--
--
--     * Modify the affinity between an instance and a <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Host> . When affinity is set to @host@ and the instance is not associated with a specific Dedicated Host, the next time the instance is launched, it is automatically associated with the host on which it lands. If the instance is restarted or rebooted, this relationship persists.
--
--     * Change the Dedicated Host with which an instance is associated.
--
--     * Change the instance tenancy of an instance from @host@ to @dedicated@ , or from @dedicated@ to @host@ .
--
--     * Move an instance to or from a <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html placement group> .
--
--
--
-- At least one attribute for affinity, host ID, tenancy, or placement group name must be specified in the request. Affinity and tenancy can be modified in the same request.
--
-- To modify the host ID, tenancy, or placement group for an instance, the instance must be in the @stopped@ state.
--
module Network.AWS.EC2.ModifyInstancePlacement
    (
    -- * Creating a Request
      modifyInstancePlacement
    , ModifyInstancePlacement
    -- * Request Lenses
    , mipAffinity
    , mipHostId
    , mipTenancy
    , mipGroupName
    , mipInstanceId

    -- * Destructuring the Response
    , modifyInstancePlacementResponse
    , ModifyInstancePlacementResponse
    -- * Response Lenses
    , miprsReturn
    , miprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyInstancePlacement.
--
--
--
-- /See:/ 'modifyInstancePlacement' smart constructor.
data ModifyInstancePlacement = ModifyInstancePlacement'
  { _mipAffinity   :: !(Maybe Affinity)
  , _mipHostId     :: !(Maybe Text)
  , _mipTenancy    :: !(Maybe HostTenancy)
  , _mipGroupName  :: !(Maybe Text)
  , _mipInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstancePlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mipAffinity' - The affinity setting for the instance.
--
-- * 'mipHostId' - The ID of the Dedicated Host with which to associate the instance.
--
-- * 'mipTenancy' - The tenancy for the instance.
--
-- * 'mipGroupName' - The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster placement groups, the instance must have a tenancy of @default@ or @dedicated@ . To remove an instance from a placement group, specify an empty string ("").
--
-- * 'mipInstanceId' - The ID of the instance that you are modifying.
modifyInstancePlacement
    :: Text -- ^ 'mipInstanceId'
    -> ModifyInstancePlacement
modifyInstancePlacement pInstanceId_ =
  ModifyInstancePlacement'
    { _mipAffinity = Nothing
    , _mipHostId = Nothing
    , _mipTenancy = Nothing
    , _mipGroupName = Nothing
    , _mipInstanceId = pInstanceId_
    }


-- | The affinity setting for the instance.
mipAffinity :: Lens' ModifyInstancePlacement (Maybe Affinity)
mipAffinity = lens _mipAffinity (\ s a -> s{_mipAffinity = a})

-- | The ID of the Dedicated Host with which to associate the instance.
mipHostId :: Lens' ModifyInstancePlacement (Maybe Text)
mipHostId = lens _mipHostId (\ s a -> s{_mipHostId = a})

-- | The tenancy for the instance.
mipTenancy :: Lens' ModifyInstancePlacement (Maybe HostTenancy)
mipTenancy = lens _mipTenancy (\ s a -> s{_mipTenancy = a})

-- | The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster placement groups, the instance must have a tenancy of @default@ or @dedicated@ . To remove an instance from a placement group, specify an empty string ("").
mipGroupName :: Lens' ModifyInstancePlacement (Maybe Text)
mipGroupName = lens _mipGroupName (\ s a -> s{_mipGroupName = a})

-- | The ID of the instance that you are modifying.
mipInstanceId :: Lens' ModifyInstancePlacement Text
mipInstanceId = lens _mipInstanceId (\ s a -> s{_mipInstanceId = a})

instance AWSRequest ModifyInstancePlacement where
        type Rs ModifyInstancePlacement =
             ModifyInstancePlacementResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyInstancePlacementResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyInstancePlacement where

instance NFData ModifyInstancePlacement where

instance ToHeaders ModifyInstancePlacement where
        toHeaders = const mempty

instance ToPath ModifyInstancePlacement where
        toPath = const "/"

instance ToQuery ModifyInstancePlacement where
        toQuery ModifyInstancePlacement'{..}
          = mconcat
              ["Action" =:
                 ("ModifyInstancePlacement" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Affinity" =: _mipAffinity, "HostId" =: _mipHostId,
               "Tenancy" =: _mipTenancy,
               "GroupName" =: _mipGroupName,
               "InstanceId" =: _mipInstanceId]

-- | Contains the output of ModifyInstancePlacement.
--
--
--
-- /See:/ 'modifyInstancePlacementResponse' smart constructor.
data ModifyInstancePlacementResponse = ModifyInstancePlacementResponse'
  { _miprsReturn         :: !(Maybe Bool)
  , _miprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstancePlacementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miprsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'miprsResponseStatus' - -- | The response status code.
modifyInstancePlacementResponse
    :: Int -- ^ 'miprsResponseStatus'
    -> ModifyInstancePlacementResponse
modifyInstancePlacementResponse pResponseStatus_ =
  ModifyInstancePlacementResponse'
    {_miprsReturn = Nothing, _miprsResponseStatus = pResponseStatus_}


-- | Is @true@ if the request succeeds, and an error otherwise.
miprsReturn :: Lens' ModifyInstancePlacementResponse (Maybe Bool)
miprsReturn = lens _miprsReturn (\ s a -> s{_miprsReturn = a})

-- | -- | The response status code.
miprsResponseStatus :: Lens' ModifyInstancePlacementResponse Int
miprsResponseStatus = lens _miprsResponseStatus (\ s a -> s{_miprsResponseStatus = a})

instance NFData ModifyInstancePlacementResponse where
