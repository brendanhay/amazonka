{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a DBSecurityGroup for previously authorized IP
-- ranges or EC2 or VPC Security Groups. Required parameters for this API
-- are one of CIDRIP, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId).
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RevokeDBSecurityGroupIngress.html>
module Network.AWS.RDS.RevokeDBSecurityGroupIngress
    (
    -- * Request
      RevokeDBSecurityGroupIngress
    -- ** Request constructor
    , revokeDBSecurityGroupIngress
    -- ** Request lenses
    , rdsgiEC2SecurityGroupOwnerId
    , rdsgiEC2SecurityGroupName
    , rdsgiCIdRIP
    , rdsgiEC2SecurityGroupId
    , rdsgiDBSecurityGroupName

    -- * Response
    , RevokeDBSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , rdsgirsDBSecurityGroup
    , rdsgirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'revokeDBSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsgiEC2SecurityGroupOwnerId'
--
-- * 'rdsgiEC2SecurityGroupName'
--
-- * 'rdsgiCIdRIP'
--
-- * 'rdsgiEC2SecurityGroupId'
--
-- * 'rdsgiDBSecurityGroupName'
data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress'
    { _rdsgiEC2SecurityGroupOwnerId :: !(Maybe Text)
    , _rdsgiEC2SecurityGroupName    :: !(Maybe Text)
    , _rdsgiCIdRIP                  :: !(Maybe Text)
    , _rdsgiEC2SecurityGroupId      :: !(Maybe Text)
    , _rdsgiDBSecurityGroupName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeDBSecurityGroupIngress' smart constructor.
revokeDBSecurityGroupIngress :: Text -> RevokeDBSecurityGroupIngress
revokeDBSecurityGroupIngress pDBSecurityGroupName_ =
    RevokeDBSecurityGroupIngress'
    { _rdsgiEC2SecurityGroupOwnerId = Nothing
    , _rdsgiEC2SecurityGroupName = Nothing
    , _rdsgiCIdRIP = Nothing
    , _rdsgiEC2SecurityGroupId = Nothing
    , _rdsgiDBSecurityGroupName = pDBSecurityGroupName_
    }

-- | The AWS Account Number of the owner of the EC2 security group specified
-- in the @EC2SecurityGroupName@ parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must
-- be provided. Otherwise, EC2SecurityGroupOwnerId and either
-- @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
rdsgiEC2SecurityGroupOwnerId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdsgiEC2SecurityGroupOwnerId = lens _rdsgiEC2SecurityGroupOwnerId (\ s a -> s{_rdsgiEC2SecurityGroupOwnerId = a});

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
rdsgiEC2SecurityGroupName :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdsgiEC2SecurityGroupName = lens _rdsgiEC2SecurityGroupName (\ s a -> s{_rdsgiEC2SecurityGroupName = a});

-- | The IP range to revoke access from. Must be a valid CIDR range. If
-- @CIDRIP@ is specified, @EC2SecurityGroupName@, @EC2SecurityGroupId@ and
-- @EC2SecurityGroupOwnerId@ cannot be provided.
rdsgiCIdRIP :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdsgiCIdRIP = lens _rdsgiCIdRIP (\ s a -> s{_rdsgiCIdRIP = a});

-- | The id of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
rdsgiEC2SecurityGroupId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdsgiEC2SecurityGroupId = lens _rdsgiEC2SecurityGroupId (\ s a -> s{_rdsgiEC2SecurityGroupId = a});

-- | The name of the DB security group to revoke ingress from.
rdsgiDBSecurityGroupName :: Lens' RevokeDBSecurityGroupIngress Text
rdsgiDBSecurityGroupName = lens _rdsgiDBSecurityGroupName (\ s a -> s{_rdsgiDBSecurityGroupName = a});

instance AWSRequest RevokeDBSecurityGroupIngress
         where
        type Sv RevokeDBSecurityGroupIngress = RDS
        type Rs RevokeDBSecurityGroupIngress =
             RevokeDBSecurityGroupIngressResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "RevokeDBSecurityGroupIngressResult"
              (\ s h x ->
                 RevokeDBSecurityGroupIngressResponse' <$>
                   (x .@? "DBSecurityGroup") <*> (pure (fromEnum s)))

instance ToHeaders RevokeDBSecurityGroupIngress where
        toHeaders = const mempty

instance ToPath RevokeDBSecurityGroupIngress where
        toPath = const "/"

instance ToQuery RevokeDBSecurityGroupIngress where
        toQuery RevokeDBSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeDBSecurityGroupIngress" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EC2SecurityGroupOwnerId" =:
                 _rdsgiEC2SecurityGroupOwnerId,
               "EC2SecurityGroupName" =: _rdsgiEC2SecurityGroupName,
               "CIDRIP" =: _rdsgiCIdRIP,
               "EC2SecurityGroupId" =: _rdsgiEC2SecurityGroupId,
               "DBSecurityGroupName" =: _rdsgiDBSecurityGroupName]

-- | /See:/ 'revokeDBSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsgirsDBSecurityGroup'
--
-- * 'rdsgirsStatus'
data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse'
    { _rdsgirsDBSecurityGroup :: !(Maybe DBSecurityGroup)
    , _rdsgirsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeDBSecurityGroupIngressResponse' smart constructor.
revokeDBSecurityGroupIngressResponse :: Int -> RevokeDBSecurityGroupIngressResponse
revokeDBSecurityGroupIngressResponse pStatus_ =
    RevokeDBSecurityGroupIngressResponse'
    { _rdsgirsDBSecurityGroup = Nothing
    , _rdsgirsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rdsgirsDBSecurityGroup :: Lens' RevokeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
rdsgirsDBSecurityGroup = lens _rdsgirsDBSecurityGroup (\ s a -> s{_rdsgirsDBSecurityGroup = a});

-- | FIXME: Undocumented member.
rdsgirsStatus :: Lens' RevokeDBSecurityGroupIngressResponse Int
rdsgirsStatus = lens _rdsgirsStatus (\ s a -> s{_rdsgirsStatus = a});
