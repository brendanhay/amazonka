{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of interconnects owned by the AWS account.
--
-- If an interconnect ID is provided, it will only return this particular
-- interconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeInterconnects.html>
module Network.AWS.DirectConnect.DescribeInterconnects
    (
    -- * Request
      DescribeInterconnects
    -- ** Request constructor
    , describeInterconnects
    -- ** Request lenses
    , dirqInterconnectId

    -- * Response
    , DescribeInterconnectsResponse
    -- ** Response constructor
    , describeInterconnectsResponse
    -- ** Response lenses
    , dirsInterconnects
    , dirsStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeInterconnects operation.
--
-- /See:/ 'describeInterconnects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirqInterconnectId'
newtype DescribeInterconnects = DescribeInterconnects'
    { _dirqInterconnectId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInterconnects' smart constructor.
describeInterconnects :: DescribeInterconnects
describeInterconnects =
    DescribeInterconnects'
    { _dirqInterconnectId = Nothing
    }

-- | FIXME: Undocumented member.
dirqInterconnectId :: Lens' DescribeInterconnects (Maybe Text)
dirqInterconnectId = lens _dirqInterconnectId (\ s a -> s{_dirqInterconnectId = a});

instance AWSRequest DescribeInterconnects where
        type Sv DescribeInterconnects = DirectConnect
        type Rs DescribeInterconnects =
             DescribeInterconnectsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInterconnectsResponse' <$>
                   (x .?> "interconnects" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeInterconnects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeInterconnects" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInterconnects where
        toJSON DescribeInterconnects'{..}
          = object ["interconnectId" .= _dirqInterconnectId]

instance ToPath DescribeInterconnects where
        toPath = const "/"

instance ToQuery DescribeInterconnects where
        toQuery = const mempty

-- | A structure containing a list of interconnects.
--
-- /See:/ 'describeInterconnectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirsInterconnects'
--
-- * 'dirsStatus'
data DescribeInterconnectsResponse = DescribeInterconnectsResponse'
    { _dirsInterconnects :: !(Maybe [Interconnect])
    , _dirsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInterconnectsResponse' smart constructor.
describeInterconnectsResponse :: Int -> DescribeInterconnectsResponse
describeInterconnectsResponse pStatus_ =
    DescribeInterconnectsResponse'
    { _dirsInterconnects = Nothing
    , _dirsStatus = pStatus_
    }

-- | A list of interconnects.
dirsInterconnects :: Lens' DescribeInterconnectsResponse [Interconnect]
dirsInterconnects = lens _dirsInterconnects (\ s a -> s{_dirsInterconnects = a}) . _Default;

-- | FIXME: Undocumented member.
dirsStatus :: Lens' DescribeInterconnectsResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});
