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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the instance affinity value for a specific stopped instance and modify the instance tenancy setting.
--
--
-- Instance affinity is disabled by default. When instance affinity is @host@ and it is not associated with a specific Dedicated Host, the next time it is launched it will automatically be associated with the host it lands on. This relationship will persist if the instance is stopped/started, or rebooted.
--
-- You can modify the host ID associated with a stopped instance. If a stopped instance has a new host ID association, the instance will target that host when restarted.
--
-- You can modify the tenancy of a stopped instance with a tenancy of @host@ or @dedicated@ .
--
-- Affinity, hostID, and tenancy are not required parameters, but at least one of them must be specified in the request. Affinity and tenancy can be modified in the same request, but tenancy can only be modified on instances that are stopped.
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
  , _mipInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstancePlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mipAffinity' - The new affinity setting for the instance.
--
-- * 'mipHostId' - The ID of the Dedicated Host that the instance will have affinity with.
--
-- * 'mipTenancy' - The tenancy of the instance that you are modifying.
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
  , _mipInstanceId = pInstanceId_
  }


-- | The new affinity setting for the instance.
mipAffinity :: Lens' ModifyInstancePlacement (Maybe Affinity)
mipAffinity = lens _mipAffinity (\ s a -> s{_mipAffinity = a});

-- | The ID of the Dedicated Host that the instance will have affinity with.
mipHostId :: Lens' ModifyInstancePlacement (Maybe Text)
mipHostId = lens _mipHostId (\ s a -> s{_mipHostId = a});

-- | The tenancy of the instance that you are modifying.
mipTenancy :: Lens' ModifyInstancePlacement (Maybe HostTenancy)
mipTenancy = lens _mipTenancy (\ s a -> s{_mipTenancy = a});

-- | The ID of the instance that you are modifying.
mipInstanceId :: Lens' ModifyInstancePlacement Text
mipInstanceId = lens _mipInstanceId (\ s a -> s{_mipInstanceId = a});

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
miprsReturn = lens _miprsReturn (\ s a -> s{_miprsReturn = a});

-- | -- | The response status code.
miprsResponseStatus :: Lens' ModifyInstancePlacementResponse Int
miprsResponseStatus = lens _miprsResponseStatus (\ s a -> s{_miprsResponseStatus = a});

instance NFData ModifyInstancePlacementResponse where
