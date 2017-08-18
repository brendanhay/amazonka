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
-- Module      : Network.AWS.EC2.ModifyIdentityIdFormat
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format of a resource for a specified IAM user, IAM role, or the root user for an account; or all IAM users, IAM roles, and the root user for an account. You can specify that resources should receive longer IDs (17-character IDs) when they are created.
--
--
-- The following resource types support longer IDs: @instance@ | @reservation@ | @snapshot@ | @volume@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This setting applies to the principal specified in the request; it does not apply to the principal that makes the request.
--
-- Resources created with longer IDs are visible to all IAM roles and users, regardless of these settings and provided that they have permission to use the relevant @Describe@ command for the resource type.
--
module Network.AWS.EC2.ModifyIdentityIdFormat
    (
    -- * Creating a Request
      modifyIdentityIdFormat
    , ModifyIdentityIdFormat
    -- * Request Lenses
    , miifPrincipalARN
    , miifResource
    , miifUseLongIds

    -- * Destructuring the Response
    , modifyIdentityIdFormatResponse
    , ModifyIdentityIdFormatResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters of ModifyIdentityIdFormat.
--
--
--
-- /See:/ 'modifyIdentityIdFormat' smart constructor.
data ModifyIdentityIdFormat = ModifyIdentityIdFormat'
    { _miifPrincipalARN :: !Text
    , _miifResource     :: !Text
    , _miifUseLongIds   :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyIdentityIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miifPrincipalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
--
-- * 'miifResource' - The type of resource: @instance@ | @reservation@ | @snapshot@ | @volume@
--
-- * 'miifUseLongIds' - Indicates whether the resource should use longer IDs (17-character IDs)
modifyIdentityIdFormat
    :: Text -- ^ 'miifPrincipalARN'
    -> Text -- ^ 'miifResource'
    -> Bool -- ^ 'miifUseLongIds'
    -> ModifyIdentityIdFormat
modifyIdentityIdFormat pPrincipalARN_ pResource_ pUseLongIds_ =
    ModifyIdentityIdFormat'
    { _miifPrincipalARN = pPrincipalARN_
    , _miifResource = pResource_
    , _miifUseLongIds = pUseLongIds_
    }

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
miifPrincipalARN :: Lens' ModifyIdentityIdFormat Text
miifPrincipalARN = lens _miifPrincipalARN (\ s a -> s{_miifPrincipalARN = a});

-- | The type of resource: @instance@ | @reservation@ | @snapshot@ | @volume@
miifResource :: Lens' ModifyIdentityIdFormat Text
miifResource = lens _miifResource (\ s a -> s{_miifResource = a});

-- | Indicates whether the resource should use longer IDs (17-character IDs)
miifUseLongIds :: Lens' ModifyIdentityIdFormat Bool
miifUseLongIds = lens _miifUseLongIds (\ s a -> s{_miifUseLongIds = a});

instance AWSRequest ModifyIdentityIdFormat where
        type Rs ModifyIdentityIdFormat =
             ModifyIdentityIdFormatResponse
        request = postQuery ec2
        response
          = receiveNull ModifyIdentityIdFormatResponse'

instance Hashable ModifyIdentityIdFormat

instance NFData ModifyIdentityIdFormat

instance ToHeaders ModifyIdentityIdFormat where
        toHeaders = const mempty

instance ToPath ModifyIdentityIdFormat where
        toPath = const "/"

instance ToQuery ModifyIdentityIdFormat where
        toQuery ModifyIdentityIdFormat'{..}
          = mconcat
              ["Action" =:
                 ("ModifyIdentityIdFormat" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "PrincipalArn" =: _miifPrincipalARN,
               "Resource" =: _miifResource,
               "UseLongIds" =: _miifUseLongIds]

-- | /See:/ 'modifyIdentityIdFormatResponse' smart constructor.
data ModifyIdentityIdFormatResponse =
    ModifyIdentityIdFormatResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyIdentityIdFormatResponse' with the minimum fields required to make a request.
--
modifyIdentityIdFormatResponse
    :: ModifyIdentityIdFormatResponse
modifyIdentityIdFormatResponse = ModifyIdentityIdFormatResponse'

instance NFData ModifyIdentityIdFormatResponse
