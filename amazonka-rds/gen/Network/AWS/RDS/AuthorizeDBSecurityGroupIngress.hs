{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables ingress to a DBSecurityGroup using one of two forms of
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
    , adsgirqEC2SecurityGroupOwnerId
    , adsgirqEC2SecurityGroupName
    , adsgirqCIdRIP
    , adsgirqEC2SecurityGroupId
    , adsgirqDBSecurityGroupName

    -- * Response
    , AuthorizeDBSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , adsgirsDBSecurityGroup
    , adsgirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'authorizeDBSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adsgirqEC2SecurityGroupOwnerId'
--
-- * 'adsgirqEC2SecurityGroupName'
--
-- * 'adsgirqCIdRIP'
--
-- * 'adsgirqEC2SecurityGroupId'
--
-- * 'adsgirqDBSecurityGroupName'
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'
    { _adsgirqEC2SecurityGroupOwnerId :: !(Maybe Text)
    , _adsgirqEC2SecurityGroupName    :: !(Maybe Text)
    , _adsgirqCIdRIP                  :: !(Maybe Text)
    , _adsgirqEC2SecurityGroupId      :: !(Maybe Text)
    , _adsgirqDBSecurityGroupName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeDBSecurityGroupIngress' smart constructor.
authorizeDBSecurityGroupIngress :: Text -> AuthorizeDBSecurityGroupIngress
authorizeDBSecurityGroupIngress pDBSecurityGroupName =
    AuthorizeDBSecurityGroupIngress'
    { _adsgirqEC2SecurityGroupOwnerId = Nothing
    , _adsgirqEC2SecurityGroupName = Nothing
    , _adsgirqCIdRIP = Nothing
    , _adsgirqEC2SecurityGroupId = Nothing
    , _adsgirqDBSecurityGroupName = pDBSecurityGroupName
    }

-- | AWS Account Number of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must
-- be provided. Otherwise, EC2SecurityGroupOwnerId and either
-- @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
adsgirqEC2SecurityGroupOwnerId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgirqEC2SecurityGroupOwnerId = lens _adsgirqEC2SecurityGroupOwnerId (\ s a -> s{_adsgirqEC2SecurityGroupOwnerId = a});

-- | Name of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
adsgirqEC2SecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgirqEC2SecurityGroupName = lens _adsgirqEC2SecurityGroupName (\ s a -> s{_adsgirqEC2SecurityGroupName = a});

-- | The IP range to authorize.
adsgirqCIdRIP :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgirqCIdRIP = lens _adsgirqCIdRIP (\ s a -> s{_adsgirqCIdRIP = a});

-- | Id of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
adsgirqEC2SecurityGroupId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adsgirqEC2SecurityGroupId = lens _adsgirqEC2SecurityGroupId (\ s a -> s{_adsgirqEC2SecurityGroupId = a});

-- | The name of the DB security group to add authorization to.
adsgirqDBSecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress Text
adsgirqDBSecurityGroupName = lens _adsgirqDBSecurityGroupName (\ s a -> s{_adsgirqDBSecurityGroupName = a});

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
                   (x .@? "DBSecurityGroup") <*> (pure (fromEnum s)))

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
                 _adsgirqEC2SecurityGroupOwnerId,
               "EC2SecurityGroupName" =:
                 _adsgirqEC2SecurityGroupName,
               "CIDRIP" =: _adsgirqCIdRIP,
               "EC2SecurityGroupId" =: _adsgirqEC2SecurityGroupId,
               "DBSecurityGroupName" =: _adsgirqDBSecurityGroupName]

-- | /See:/ 'authorizeDBSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adsgirsDBSecurityGroup'
--
-- * 'adsgirsStatus'
data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'
    { _adsgirsDBSecurityGroup :: !(Maybe DBSecurityGroup)
    , _adsgirsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeDBSecurityGroupIngressResponse' smart constructor.
authorizeDBSecurityGroupIngressResponse :: Int -> AuthorizeDBSecurityGroupIngressResponse
authorizeDBSecurityGroupIngressResponse pStatus =
    AuthorizeDBSecurityGroupIngressResponse'
    { _adsgirsDBSecurityGroup = Nothing
    , _adsgirsStatus = pStatus
    }

-- | FIXME: Undocumented member.
adsgirsDBSecurityGroup :: Lens' AuthorizeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
adsgirsDBSecurityGroup = lens _adsgirsDBSecurityGroup (\ s a -> s{_adsgirsDBSecurityGroup = a});

-- | FIXME: Undocumented member.
adsgirsStatus :: Lens' AuthorizeDBSecurityGroupIngressResponse Int
adsgirsStatus = lens _adsgirsStatus (\ s a -> s{_adsgirsStatus = a});
