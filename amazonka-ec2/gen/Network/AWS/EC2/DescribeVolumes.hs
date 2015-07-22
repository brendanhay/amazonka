{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS volumes.
--
-- If you are describing a long list of volumes, you can paginate the
-- output to make the list more manageable. The @MaxResults@ parameter sets
-- the maximum number of results returned in a single page. If the list of
-- results exceeds your @MaxResults@ value, then that number of results is
-- returned along with a @NextToken@ value that can be passed to a
-- subsequent @DescribeVolumes@ request to retrieve the remaining results.
--
-- For more information about EBS volumes, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html>
module Network.AWS.EC2.DescribeVolumes
    (
    -- * Request
      DescribeVolumes
    -- ** Request constructor
    , describeVolumes
    -- ** Request lenses
    , desrqFilters
    , desrqVolumeIds
    , desrqNextToken
    , desrqDryRun
    , desrqMaxResults

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , desrsNextToken
    , desrsVolumes
    , desrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrqFilters'
--
-- * 'desrqVolumeIds'
--
-- * 'desrqNextToken'
--
-- * 'desrqDryRun'
--
-- * 'desrqMaxResults'
data DescribeVolumes = DescribeVolumes'
    { _desrqFilters    :: !(Maybe [Filter])
    , _desrqVolumeIds  :: !(Maybe [Text])
    , _desrqNextToken  :: !(Maybe Text)
    , _desrqDryRun     :: !(Maybe Bool)
    , _desrqMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumes' smart constructor.
describeVolumes :: DescribeVolumes
describeVolumes =
    DescribeVolumes'
    { _desrqFilters = Nothing
    , _desrqVolumeIds = Nothing
    , _desrqNextToken = Nothing
    , _desrqDryRun = Nothing
    , _desrqMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   @attachment.attach-time@ - The time stamp when the attachment
--     initiated.
--
-- -   @attachment.delete-on-termination@ - Whether the volume is deleted
--     on instance termination.
--
-- -   @attachment.device@ - The device name that is exposed to the
--     instance (for example, @\/dev\/sda1@).
--
-- -   @attachment.instance-id@ - The ID of the instance the volume is
--     attached to.
--
-- -   @attachment.status@ - The attachment state (@attaching@ | @attached@
--     | @detaching@ | @detached@).
--
-- -   @availability-zone@ - The Availability Zone in which the volume was
--     created.
--
-- -   @create-time@ - The time stamp when the volume was created.
--
-- -   @encrypted@ - The encryption status of the volume.
--
-- -   @size@ - The size of the volume, in GiB.
--
-- -   @snapshot-id@ - The snapshot from which the volume was created.
--
-- -   @status@ - The status of the volume (@creating@ | @available@ |
--     @in-use@ | @deleting@ | @deleted@ | @error@).
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the @tag-value@ filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
-- -   @volume-id@ - The volume ID.
--
-- -   @volume-type@ - The Amazon EBS volume type. This can be @gp2@ for
--     General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD)
--     volumes, or @standard@ for Magnetic volumes.
--
desrqFilters :: Lens' DescribeVolumes [Filter]
desrqFilters = lens _desrqFilters (\ s a -> s{_desrqFilters = a}) . _Default;

-- | One or more volume IDs.
desrqVolumeIds :: Lens' DescribeVolumes [Text]
desrqVolumeIds = lens _desrqVolumeIds (\ s a -> s{_desrqVolumeIds = a}) . _Default;

-- | The @NextToken@ value returned from a previous paginated
-- @DescribeVolumes@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
desrqNextToken :: Lens' DescribeVolumes (Maybe Text)
desrqNextToken = lens _desrqNextToken (\ s a -> s{_desrqNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
desrqDryRun :: Lens' DescribeVolumes (Maybe Bool)
desrqDryRun = lens _desrqDryRun (\ s a -> s{_desrqDryRun = a});

-- | The maximum number of volume results returned by @DescribeVolumes@ in
-- paginated output. When this parameter is used, @DescribeVolumes@ only
-- returns @MaxResults@ results in a single page along with a @NextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeVolumes@ request with the returned
-- @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@
-- is given a value larger than 1000, only 1000 results are returned. If
-- this parameter is not used, then @DescribeVolumes@ returns all results.
-- You cannot specify this parameter and the volume IDs parameter in the
-- same request.
desrqMaxResults :: Lens' DescribeVolumes (Maybe Int)
desrqMaxResults = lens _desrqMaxResults (\ s a -> s{_desrqMaxResults = a});

instance AWSRequest DescribeVolumes where
        type Sv DescribeVolumes = EC2
        type Rs DescribeVolumes = DescribeVolumesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVolumesResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "volumeSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVolumes where
        toHeaders = const mempty

instance ToPath DescribeVolumes where
        toPath = const "/"

instance ToQuery DescribeVolumes where
        toQuery DescribeVolumes'{..}
          = mconcat
              ["Action" =: ("DescribeVolumes" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _desrqFilters),
               toQuery (toQueryList "VolumeId" <$> _desrqVolumeIds),
               "NextToken" =: _desrqNextToken,
               "DryRun" =: _desrqDryRun,
               "MaxResults" =: _desrqMaxResults]

-- | /See:/ 'describeVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrsNextToken'
--
-- * 'desrsVolumes'
--
-- * 'desrsStatus'
data DescribeVolumesResponse = DescribeVolumesResponse'
    { _desrsNextToken :: !(Maybe Text)
    , _desrsVolumes   :: !(Maybe [Volume])
    , _desrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumesResponse' smart constructor.
describeVolumesResponse :: Int -> DescribeVolumesResponse
describeVolumesResponse pStatus =
    DescribeVolumesResponse'
    { _desrsNextToken = Nothing
    , _desrsVolumes = Nothing
    , _desrsStatus = pStatus
    }

-- | The @NextToken@ value to include in a future @DescribeVolumes@ request.
-- When the results of a @DescribeVolumes@ request exceed @MaxResults@,
-- this value can be used to retrieve the next page of results. This value
-- is @null@ when there are no more results to return.
desrsNextToken :: Lens' DescribeVolumesResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\ s a -> s{_desrsNextToken = a});

-- | Information about the volumes.
desrsVolumes :: Lens' DescribeVolumesResponse [Volume]
desrsVolumes = lens _desrsVolumes (\ s a -> s{_desrsVolumes = a}) . _Default;

-- | FIXME: Undocumented member.
desrsStatus :: Lens' DescribeVolumesResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});
