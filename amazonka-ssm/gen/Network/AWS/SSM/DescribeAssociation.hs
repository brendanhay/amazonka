{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations for the specified configuration document or
-- instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DescribeAssociation.html>
module Network.AWS.SSM.DescribeAssociation
    (
    -- * Request
      DescribeAssociation
    -- ** Request constructor
    , describeAssociation
    -- ** Request lenses
    , darqName
    , darqInstanceId

    -- * Response
    , DescribeAssociationResponse
    -- ** Response constructor
    , describeAssociationResponse
    -- ** Response lenses
    , darsAssociationDescription
    , darsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'describeAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darqName'
--
-- * 'darqInstanceId'
data DescribeAssociation = DescribeAssociation'
    { _darqName       :: !Text
    , _darqInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAssociation' smart constructor.
describeAssociation :: Text -> Text -> DescribeAssociation
describeAssociation pName_ pInstanceId_ =
    DescribeAssociation'
    { _darqName = pName_
    , _darqInstanceId = pInstanceId_
    }

-- | The name of the configuration document.
darqName :: Lens' DescribeAssociation Text
darqName = lens _darqName (\ s a -> s{_darqName = a});

-- | The ID of the instance.
darqInstanceId :: Lens' DescribeAssociation Text
darqInstanceId = lens _darqInstanceId (\ s a -> s{_darqInstanceId = a});

instance AWSRequest DescribeAssociation where
        type Sv DescribeAssociation = SSM
        type Rs DescribeAssociation =
             DescribeAssociationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeAssociation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeAssociation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssociation where
        toJSON DescribeAssociation'{..}
          = object
              ["Name" .= _darqName,
               "InstanceId" .= _darqInstanceId]

instance ToPath DescribeAssociation where
        toPath = const "/"

instance ToQuery DescribeAssociation where
        toQuery = const mempty

-- | /See:/ 'describeAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darsAssociationDescription'
--
-- * 'darsStatus'
data DescribeAssociationResponse = DescribeAssociationResponse'
    { _darsAssociationDescription :: !(Maybe AssociationDescription)
    , _darsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAssociationResponse' smart constructor.
describeAssociationResponse :: Int -> DescribeAssociationResponse
describeAssociationResponse pStatus_ =
    DescribeAssociationResponse'
    { _darsAssociationDescription = Nothing
    , _darsStatus = pStatus_
    }

-- | Information about the association.
darsAssociationDescription :: Lens' DescribeAssociationResponse (Maybe AssociationDescription)
darsAssociationDescription = lens _darsAssociationDescription (\ s a -> s{_darsAssociationDescription = a});

-- | FIXME: Undocumented member.
darsStatus :: Lens' DescribeAssociationResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});
