{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables ingress to a DBSecurityGroup using one of two forms of
-- authorization. First, EC2 or VPC security groups can be added to the
-- DBSecurityGroup if the application using the database is running on EC2
-- or VPC instances. Second, IP ranges are available if the application
-- accessing your database is running on the Internet. Required parameters
-- for this API are one of CIDR range, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId for non-VPC).
--
-- You cannot authorize ingress from an EC2 security group in one Region to
-- an Amazon RDS DB instance in another. You cannot authorize ingress from
-- a VPC security group in one VPC to an Amazon RDS DB instance in another.
--
-- For an overview of CIDR ranges, go to the
-- <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial>.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_AuthorizeDBSecurityGroupIngress.html>
module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
    (
    -- * Request
      AuthorizeDBSecurityGroupIngress
    -- ** Request constructor
    , authorizeDBSecurityGroupIngress
    -- ** Request lenses
    , adsgiEC2SecurityGroupOwnerId
    , adsgiEC2SecurityGroupName
    , adsgiCIDRIP
    , adsgiEC2SecurityGroupId
    , adsgiDBSecurityGroupName

    -- * Response
    , AuthorizeDBSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , adsgirDBSecurityGroup
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'authorizeDBSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adsgiEC2SecurityGroupOwnerId'
--
-- * 'adsgiEC2SecurityGroupName'
--
-- * 'adsgiCIDRIP'
--
-- * 'adsgiEC2SecurityGroupId'
--
-- * 'adsgiDBSecurityGroupName'
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'{_adsgiEC2SecurityGroupOwnerId :: Maybe Text, _adsgiEC2SecurityGroupName :: Maybe Text, _adsgiCIDRIP :: Maybe Text, _adsgiEC2SecurityGroupId :: Maybe Text, _adsgiDBSecurityGroupName :: Text} deriving (Eq, Read, Show)

-- | 'AuthorizeDBSecurityGroupIngress' smart constructor.
authorizeDBSecurityGroupIngress :: Text -> AuthorizeDBSecurityGroupIngress
authorizeDBSecurityGroupIngress pDBSecurityGroupName = AuthorizeDBSecurityGroupIngress'{_adsgiEC2SecurityGroupOwnerId = Nothing, _adsgiEC2SecurityGroupName = Nothing, _adsgiCIDRIP = Nothing, _adsgiEC2SecurityGroupId = Nothing, _adsgiDBSecurityGroupName = pDBSecurityGroupName};

-- | AWS Account Number of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must
-- be provided. Otherwise, EC2SecurityGroupOwnerId and either
-- @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
adsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiEC2SecurityGroupOwnerId = lens _adsgiEC2SecurityGroupOwnerId (\ s a -> s{_adsgiEC2SecurityGroupOwnerId = a});

-- | Name of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
adsgiEC2SecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiEC2SecurityGroupName = lens _adsgiEC2SecurityGroupName (\ s a -> s{_adsgiEC2SecurityGroupName = a});

-- | The IP range to authorize.
adsgiCIDRIP :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiCIDRIP = lens _adsgiCIDRIP (\ s a -> s{_adsgiCIDRIP = a});

-- | Id of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
adsgiEC2SecurityGroupId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgiEC2SecurityGroupId = lens _adsgiEC2SecurityGroupId (\ s a -> s{_adsgiEC2SecurityGroupId = a});

-- | The name of the DB security group to add authorization to.
adsgiDBSecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress Text
adsgiDBSecurityGroupName = lens _adsgiDBSecurityGroupName (\ s a -> s{_adsgiDBSecurityGroupName = a});

instance AWSRequest AuthorizeDBSecurityGroupIngress
         where
        type Sv AuthorizeDBSecurityGroupIngress = RDS
        type Rs AuthorizeDBSecurityGroupIngress =
             AuthorizeDBSecurityGroupIngressResponse
        request = post
        response
          = receiveXMLWrapper
              "AuthorizeDBSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeDBSecurityGroupIngressResponse' <$>
                   (x .@? "DBSecurityGroup"))

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
               "CIDRIP" =: _adsgiCIDRIP,
               "EC2SecurityGroupId" =: _adsgiEC2SecurityGroupId,
               "DBSecurityGroupName" =: _adsgiDBSecurityGroupName]

-- | /See:/ 'authorizeDBSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adsgirDBSecurityGroup'
newtype AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'{_adsgirDBSecurityGroup :: Maybe DBSecurityGroup} deriving (Eq, Read, Show)

-- | 'AuthorizeDBSecurityGroupIngressResponse' smart constructor.
authorizeDBSecurityGroupIngressResponse :: AuthorizeDBSecurityGroupIngressResponse
authorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'{_adsgirDBSecurityGroup = Nothing};

-- | FIXME: Undocumented member.
adsgirDBSecurityGroup :: Lens' AuthorizeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
adsgirDBSecurityGroup = lens _adsgirDBSecurityGroup (\ s a -> s{_adsgirDBSecurityGroup = a});
