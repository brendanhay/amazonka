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
-- Module      : Network.AWS.EC2.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets permission settings for the specified snapshot.
--
--
-- For more information on modifying snapshot permissions, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing Snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.ResetSnapshotAttribute
    (
    -- * Creating a Request
      resetSnapshotAttribute
    , ResetSnapshotAttribute
    -- * Request Lenses
    , rsaDryRun
    , rsaAttribute
    , rsaSnapshotId

    -- * Destructuring the Response
    , resetSnapshotAttributeResponse
    , ResetSnapshotAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ResetSnapshotAttribute.
--
--
--
-- /See:/ 'resetSnapshotAttribute' smart constructor.
data ResetSnapshotAttribute = ResetSnapshotAttribute'
  { _rsaDryRun     :: !(Maybe Bool)
  , _rsaAttribute  :: !SnapshotAttributeName
  , _rsaSnapshotId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rsaAttribute' - The attribute to reset. Currently, only the attribute for permission to create volumes can be reset.
--
-- * 'rsaSnapshotId' - The ID of the snapshot.
resetSnapshotAttribute
    :: SnapshotAttributeName -- ^ 'rsaAttribute'
    -> Text -- ^ 'rsaSnapshotId'
    -> ResetSnapshotAttribute
resetSnapshotAttribute pAttribute_ pSnapshotId_ =
  ResetSnapshotAttribute'
    { _rsaDryRun = Nothing
    , _rsaAttribute = pAttribute_
    , _rsaSnapshotId = pSnapshotId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsaDryRun :: Lens' ResetSnapshotAttribute (Maybe Bool)
rsaDryRun = lens _rsaDryRun (\ s a -> s{_rsaDryRun = a})

-- | The attribute to reset. Currently, only the attribute for permission to create volumes can be reset.
rsaAttribute :: Lens' ResetSnapshotAttribute SnapshotAttributeName
rsaAttribute = lens _rsaAttribute (\ s a -> s{_rsaAttribute = a})

-- | The ID of the snapshot.
rsaSnapshotId :: Lens' ResetSnapshotAttribute Text
rsaSnapshotId = lens _rsaSnapshotId (\ s a -> s{_rsaSnapshotId = a})

instance AWSRequest ResetSnapshotAttribute where
        type Rs ResetSnapshotAttribute =
             ResetSnapshotAttributeResponse
        request = postQuery ec2
        response
          = receiveNull ResetSnapshotAttributeResponse'

instance Hashable ResetSnapshotAttribute where

instance NFData ResetSnapshotAttribute where

instance ToHeaders ResetSnapshotAttribute where
        toHeaders = const mempty

instance ToPath ResetSnapshotAttribute where
        toPath = const "/"

instance ToQuery ResetSnapshotAttribute where
        toQuery ResetSnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ResetSnapshotAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _rsaDryRun, "Attribute" =: _rsaAttribute,
               "SnapshotId" =: _rsaSnapshotId]

-- | /See:/ 'resetSnapshotAttributeResponse' smart constructor.
data ResetSnapshotAttributeResponse =
  ResetSnapshotAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetSnapshotAttributeResponse' with the minimum fields required to make a request.
--
resetSnapshotAttributeResponse
    :: ResetSnapshotAttributeResponse
resetSnapshotAttributeResponse = ResetSnapshotAttributeResponse'


instance NFData ResetSnapshotAttributeResponse where
