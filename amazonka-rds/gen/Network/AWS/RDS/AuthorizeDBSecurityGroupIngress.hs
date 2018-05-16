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
-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables ingress to a DBSecurityGroup using one of two forms of authorization. First, EC2 or VPC security groups can be added to the DBSecurityGroup if the application using the database is running on EC2 or VPC instances. Second, IP ranges are available if the application accessing your database is running on the Internet. Required parameters for this API are one of CIDR range, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or EC2SecurityGroupId for non-VPC).
--
--
-- For an overview of CIDR ranges, go to the <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial> .
--
module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
    (
    -- * Creating a Request
      authorizeDBSecurityGroupIngress
    , AuthorizeDBSecurityGroupIngress
    -- * Request Lenses
    , adsgiEC2SecurityGroupOwnerId
    , adsgiEC2SecurityGroupName
    , adsgiCIdRIP
    , adsgiEC2SecurityGroupId
    , adsgiDBSecurityGroupName

    -- * Destructuring the Response
    , authorizeDBSecurityGroupIngressResponse
    , AuthorizeDBSecurityGroupIngressResponse
    -- * Response Lenses
    , adsgirsDBSecurityGroup
    , adsgirsResponseStatus
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
-- /See:/ 'authorizeDBSecurityGroupIngress' smart constructor.
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'
  { _adsgiEC2SecurityGroupOwnerId :: !(Maybe Text)
  , _adsgiEC2SecurityGroupName    :: !(Maybe Text)
  , _adsgiCIdRIP                  :: !(Maybe Text)
  , _adsgiEC2SecurityGroupId      :: !(Maybe Text)
  , _adsgiDBSecurityGroupName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeDBSecurityGroupIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adsgiEC2SecurityGroupOwnerId' - AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS Access Key ID is not an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- * 'adsgiEC2SecurityGroupName' - Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- * 'adsgiCIdRIP' - The IP range to authorize.
--
-- * 'adsgiEC2SecurityGroupId' - Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- * 'adsgiDBSecurityGroupName' - The name of the DB security group to add authorization to.
authorizeDBSecurityGroupIngress
    :: Text -- ^ 'adsgiDBSecurityGroupName'
    -> AuthorizeDBSecurityGroupIngress
authorizeDBSecurityGroupIngress pDBSecurityGroupName_ =
  AuthorizeDBSecurityGroupIngress'
    { _adsgiEC2SecurityGroupOwnerId = Nothing
    , _adsgiEC2SecurityGroupName = Nothing
    , _adsgiCIdRIP = Nothing
    , _adsgiEC2SecurityGroupId = Nothing
    , _adsgiDBSecurityGroupName = pDBSecurityGroupName_
    }


-- | AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS Access Key ID is not an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
adsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiEC2SecurityGroupOwnerId = lens _adsgiEC2SecurityGroupOwnerId (\ s a -> s{_adsgiEC2SecurityGroupOwnerId = a})

-- | Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
adsgiEC2SecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiEC2SecurityGroupName = lens _adsgiEC2SecurityGroupName (\ s a -> s{_adsgiEC2SecurityGroupName = a})

-- | The IP range to authorize.
adsgiCIdRIP :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiCIdRIP = lens _adsgiCIdRIP (\ s a -> s{_adsgiCIdRIP = a})

-- | Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
adsgiEC2SecurityGroupId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiEC2SecurityGroupId = lens _adsgiEC2SecurityGroupId (\ s a -> s{_adsgiEC2SecurityGroupId = a})

-- | The name of the DB security group to add authorization to.
adsgiDBSecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress Text
adsgiDBSecurityGroupName = lens _adsgiDBSecurityGroupName (\ s a -> s{_adsgiDBSecurityGroupName = a})

instance AWSRequest AuthorizeDBSecurityGroupIngress
         where
        type Rs AuthorizeDBSecurityGroupIngress =
             AuthorizeDBSecurityGroupIngressResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "AuthorizeDBSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeDBSecurityGroupIngressResponse' <$>
                   (x .@? "DBSecurityGroup") <*> (pure (fromEnum s)))

instance Hashable AuthorizeDBSecurityGroupIngress
         where

instance NFData AuthorizeDBSecurityGroupIngress where

instance ToHeaders AuthorizeDBSecurityGroupIngress
         where
        toHeaders = const mempty

instance ToPath AuthorizeDBSecurityGroupIngress where
        toPath = const "/"

instance ToQuery AuthorizeDBSecurityGroupIngress
         where
        toQuery AuthorizeDBSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeDBSecurityGroupIngress" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EC2SecurityGroupOwnerId" =:
                 _adsgiEC2SecurityGroupOwnerId,
               "EC2SecurityGroupName" =: _adsgiEC2SecurityGroupName,
               "CIDRIP" =: _adsgiCIdRIP,
               "EC2SecurityGroupId" =: _adsgiEC2SecurityGroupId,
               "DBSecurityGroupName" =: _adsgiDBSecurityGroupName]

-- | /See:/ 'authorizeDBSecurityGroupIngressResponse' smart constructor.
data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'
  { _adsgirsDBSecurityGroup :: !(Maybe DBSecurityGroup)
  , _adsgirsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeDBSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adsgirsDBSecurityGroup' - Undocumented member.
--
-- * 'adsgirsResponseStatus' - -- | The response status code.
authorizeDBSecurityGroupIngressResponse
    :: Int -- ^ 'adsgirsResponseStatus'
    -> AuthorizeDBSecurityGroupIngressResponse
authorizeDBSecurityGroupIngressResponse pResponseStatus_ =
  AuthorizeDBSecurityGroupIngressResponse'
    { _adsgirsDBSecurityGroup = Nothing
    , _adsgirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
adsgirsDBSecurityGroup :: Lens' AuthorizeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
adsgirsDBSecurityGroup = lens _adsgirsDBSecurityGroup (\ s a -> s{_adsgirsDBSecurityGroup = a})

-- | -- | The response status code.
adsgirsResponseStatus :: Lens' AuthorizeDBSecurityGroupIngressResponse Int
adsgirsResponseStatus = lens _adsgirsResponseStatus (\ s a -> s{_adsgirsResponseStatus = a})

instance NFData
           AuthorizeDBSecurityGroupIngressResponse
         where
