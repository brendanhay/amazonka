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
    , desFilters
    , desVolumeIds
    , desNextToken
    , desDryRun
    , desMaxResults

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvvrsNextToken
    , dvvrsVolumes
    , dvvrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desFilters'
--
-- * 'desVolumeIds'
--
-- * 'desNextToken'
--
-- * 'desDryRun'
--
-- * 'desMaxResults'
data DescribeVolumes = DescribeVolumes'
    { _desFilters    :: !(Maybe [Filter])
    , _desVolumeIds  :: !(Maybe [Text])
    , _desNextToken  :: !(Maybe Text)
    , _desDryRun     :: !(Maybe Bool)
    , _desMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumes' smart constructor.
describeVolumes :: DescribeVolumes
describeVolumes =
    DescribeVolumes'
    { _desFilters = Nothing
    , _desVolumeIds = Nothing
    , _desNextToken = Nothing
    , _desDryRun = Nothing
    , _desMaxResults = Nothing
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
desFilters :: Lens' DescribeVolumes [Filter]
desFilters = lens _desFilters (\ s a -> s{_desFilters = a}) . _Default;

-- | One or more volume IDs.
desVolumeIds :: Lens' DescribeVolumes [Text]
desVolumeIds = lens _desVolumeIds (\ s a -> s{_desVolumeIds = a}) . _Default;

-- | The @NextToken@ value returned from a previous paginated
-- @DescribeVolumes@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
desNextToken :: Lens' DescribeVolumes (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
desDryRun :: Lens' DescribeVolumes (Maybe Bool)
desDryRun = lens _desDryRun (\ s a -> s{_desDryRun = a});

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
desMaxResults :: Lens' DescribeVolumes (Maybe Int)
desMaxResults = lens _desMaxResults (\ s a -> s{_desMaxResults = a});

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
               toQuery (toQueryList "Filter" <$> _desFilters),
               toQuery (toQueryList "VolumeId" <$> _desVolumeIds),
               "NextToken" =: _desNextToken, "DryRun" =: _desDryRun,
               "MaxResults" =: _desMaxResults]

-- | /See:/ 'describeVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvvrsNextToken'
--
-- * 'dvvrsVolumes'
--
-- * 'dvvrsStatus'
data DescribeVolumesResponse = DescribeVolumesResponse'
    { _dvvrsNextToken :: !(Maybe Text)
    , _dvvrsVolumes   :: !(Maybe [Volume])
    , _dvvrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumesResponse' smart constructor.
describeVolumesResponse :: Int -> DescribeVolumesResponse
describeVolumesResponse pStatus_ =
    DescribeVolumesResponse'
    { _dvvrsNextToken = Nothing
    , _dvvrsVolumes = Nothing
    , _dvvrsStatus = pStatus_
    }

-- | The @NextToken@ value to include in a future @DescribeVolumes@ request.
-- When the results of a @DescribeVolumes@ request exceed @MaxResults@,
-- this value can be used to retrieve the next page of results. This value
-- is @null@ when there are no more results to return.
dvvrsNextToken :: Lens' DescribeVolumesResponse (Maybe Text)
dvvrsNextToken = lens _dvvrsNextToken (\ s a -> s{_dvvrsNextToken = a});

-- | Information about the volumes.
dvvrsVolumes :: Lens' DescribeVolumesResponse [Volume]
dvvrsVolumes = lens _dvvrsVolumes (\ s a -> s{_dvvrsVolumes = a}) . _Default;

-- | FIXME: Undocumented member.
dvvrsStatus :: Lens' DescribeVolumesResponse Int
dvvrsStatus = lens _dvvrsStatus (\ s a -> s{_dvvrsStatus = a});
