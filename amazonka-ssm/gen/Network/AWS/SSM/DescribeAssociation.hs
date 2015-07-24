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
    , daName
    , daInstanceId

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
-- * 'daName'
--
-- * 'daInstanceId'
data DescribeAssociation = DescribeAssociation'
    { _daName       :: !Text
    , _daInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAssociation' smart constructor.
describeAssociation :: Text -> Text -> DescribeAssociation
describeAssociation pName_ pInstanceId_ =
    DescribeAssociation'
    { _daName = pName_
    , _daInstanceId = pInstanceId_
    }

-- | The name of the configuration document.
daName :: Lens' DescribeAssociation Text
daName = lens _daName (\ s a -> s{_daName = a});

-- | The ID of the instance.
daInstanceId :: Lens' DescribeAssociation Text
daInstanceId = lens _daInstanceId (\ s a -> s{_daInstanceId = a});

instance AWSRequest DescribeAssociation where
        type Sv DescribeAssociation = SSM
        type Rs DescribeAssociation =
             DescribeAssociationResponse
        request = postJSON "DescribeAssociation"
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
              ["Name" .= _daName, "InstanceId" .= _daInstanceId]

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
