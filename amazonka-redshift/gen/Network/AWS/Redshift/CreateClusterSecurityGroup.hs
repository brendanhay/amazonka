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
-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift security group. You use security groups to control access to non-VPC clusters.
--
--
-- For information about managing security groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.CreateClusterSecurityGroup
    (
    -- * Creating a Request
      createClusterSecurityGroup
    , CreateClusterSecurityGroup
    -- * Request Lenses
    , creTags
    , creClusterSecurityGroupName
    , creDescription

    -- * Destructuring the Response
    , createClusterSecurityGroupResponse
    , CreateClusterSecurityGroupResponse
    -- * Response Lenses
    , crsClusterSecurityGroup
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createClusterSecurityGroup' smart constructor.
data CreateClusterSecurityGroup = CreateClusterSecurityGroup'
  { _creTags                     :: !(Maybe [Tag])
  , _creClusterSecurityGroupName :: !Text
  , _creDescription              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creTags' - A list of tag instances.
--
-- * 'creClusterSecurityGroupName' - The name for the security group. Amazon Redshift stores the value as a lowercase string. Constraints:     * Must contain no more than 255 alphanumeric characters or hyphens.     * Must not be "Default".     * Must be unique for all security groups that are created by your AWS account. Example: @examplesecuritygroup@
--
-- * 'creDescription' - A description for the security group.
createClusterSecurityGroup
    :: Text -- ^ 'creClusterSecurityGroupName'
    -> Text -- ^ 'creDescription'
    -> CreateClusterSecurityGroup
createClusterSecurityGroup pClusterSecurityGroupName_ pDescription_ =
  CreateClusterSecurityGroup'
    { _creTags = Nothing
    , _creClusterSecurityGroupName = pClusterSecurityGroupName_
    , _creDescription = pDescription_
    }


-- | A list of tag instances.
creTags :: Lens' CreateClusterSecurityGroup [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a}) . _Default . _Coerce

-- | The name for the security group. Amazon Redshift stores the value as a lowercase string. Constraints:     * Must contain no more than 255 alphanumeric characters or hyphens.     * Must not be "Default".     * Must be unique for all security groups that are created by your AWS account. Example: @examplesecuritygroup@
creClusterSecurityGroupName :: Lens' CreateClusterSecurityGroup Text
creClusterSecurityGroupName = lens _creClusterSecurityGroupName (\ s a -> s{_creClusterSecurityGroupName = a})

-- | A description for the security group.
creDescription :: Lens' CreateClusterSecurityGroup Text
creDescription = lens _creDescription (\ s a -> s{_creDescription = a})

instance AWSRequest CreateClusterSecurityGroup where
        type Rs CreateClusterSecurityGroup =
             CreateClusterSecurityGroupResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "CreateClusterSecurityGroupResult"
              (\ s h x ->
                 CreateClusterSecurityGroupResponse' <$>
                   (x .@? "ClusterSecurityGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateClusterSecurityGroup where

instance NFData CreateClusterSecurityGroup where

instance ToHeaders CreateClusterSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateClusterSecurityGroup where
        toPath = const "/"

instance ToQuery CreateClusterSecurityGroup where
        toQuery CreateClusterSecurityGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateClusterSecurityGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _creTags),
               "ClusterSecurityGroupName" =:
                 _creClusterSecurityGroupName,
               "Description" =: _creDescription]

-- | /See:/ 'createClusterSecurityGroupResponse' smart constructor.
data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse'
  { _crsClusterSecurityGroup :: !(Maybe ClusterSecurityGroup)
  , _crsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterSecurityGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsClusterSecurityGroup' - Undocumented member.
--
-- * 'crsResponseStatus' - -- | The response status code.
createClusterSecurityGroupResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateClusterSecurityGroupResponse
createClusterSecurityGroupResponse pResponseStatus_ =
  CreateClusterSecurityGroupResponse'
    {_crsClusterSecurityGroup = Nothing, _crsResponseStatus = pResponseStatus_}


-- | Undocumented member.
crsClusterSecurityGroup :: Lens' CreateClusterSecurityGroupResponse (Maybe ClusterSecurityGroup)
crsClusterSecurityGroup = lens _crsClusterSecurityGroup (\ s a -> s{_crsClusterSecurityGroup = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateClusterSecurityGroupResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateClusterSecurityGroupResponse
         where
