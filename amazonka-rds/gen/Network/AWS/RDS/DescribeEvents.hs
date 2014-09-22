{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns events related to DB instances, DB security groups, DB snapshots,
-- and DB parameter groups for the past 14 days. Events specific to a
-- particular DB instance, DB security group, database snapshot, or DB
-- parameter group can be obtained by providing the name as a parameter. By
-- default, the past hour of events are returned. https://rds.amazonaws.com/
-- ?Action=DescribeEvents &Duration=1440 &MaxRecords=100 &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T20%3A00%3A44.420Z &AWSAccessKeyId= &Signature=
-- Applied change to security group db-security-group 2010-08-11T17:12:52.860Z
-- mydbsecuritygroup Database instance created db-instance
-- 2010-08-11T18:10:15.269Z mydbinstance3 Backing up database instance
-- db-instance 2010-08-11T18:10:34.690Z mydbinstance3 Backing up DB instance
-- db-instance 2010-08-11T18:25:52.263Z mynewdbinstance Creating user snapshot
-- db-snapshot 2010-08-11T18:25:52.263Z mynewdbsnapshot3
-- 95b948cd-bf45-11de-86a4-97241dfaadff.
module Network.AWS.RDS.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , deSourceIdentifier
    , deSourceType
    , deStartTime
    , deEndTime
    , deDuration
    , deEventCategories
    , deMaxRecords
    , deMarker

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , derMarker
    , derEvents
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeEvents = DescribeEvents
    { _deSourceIdentifier :: Maybe Text
    , _deSourceType :: Maybe SourceType
    , _deStartTime :: Maybe ISO8601
    , _deEndTime :: Maybe ISO8601
    , _deDuration :: Maybe Integer
    , _deEventCategories :: [Text]
    , _deMaxRecords :: Maybe Integer
    , _deMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEvents' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceIdentifier ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe SourceType@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @EventCategories ::@ @[Text]@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _deSourceIdentifier = Nothing
    , _deSourceType = Nothing
    , _deStartTime = Nothing
    , _deEndTime = Nothing
    , _deDuration = Nothing
    , _deEventCategories = mempty
    , _deMaxRecords = Nothing
    , _deMarker = Nothing
    }

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response. Constraints:
-- If SourceIdentifier is supplied, SourceType must also be provided. If the
-- source type is DBInstance, then a DBInstanceIdentifier must be supplied. If
-- the source type is DBSecurityGroup, a DBSecurityGroupName must be supplied.
-- If the source type is DBParameterGroup, a DBParameterGroupName must be
-- supplied. If the source type is DBSnapshot, a DBSnapshotIdentifier must be
-- supplied. Cannot end with a hyphen or contain two consecutive hyphens.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier =
    lens _deSourceIdentifier (\s a -> s { _deSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\s a -> s { _deSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
deStartTime :: Lens' DescribeEvents (Maybe ISO8601)
deStartTime = lens _deStartTime (\s a -> s { _deStartTime = a })

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
deEndTime :: Lens' DescribeEvents (Maybe ISO8601)
deEndTime = lens _deEndTime (\s a -> s { _deEndTime = a })

-- | The number of minutes to retrieve events for. Default: 60.
deDuration :: Lens' DescribeEvents (Maybe Integer)
deDuration = lens _deDuration (\s a -> s { _deDuration = a })

-- | A list of event categories that trigger notifications for a event
-- notification subscription.
deEventCategories :: Lens' DescribeEvents [Text]
deEventCategories =
    lens _deEventCategories (\s a -> s { _deEventCategories = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Integer)
deMaxRecords = lens _deMaxRecords (\s a -> s { _deMaxRecords = a })

-- | An optional pagination token provided by a previous DescribeEvents request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords.
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\s a -> s { _deMarker = a })

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the DescribeEvents
-- action.
data DescribeEventsResponse = DescribeEventsResponse
    { _derMarker :: Maybe Text
    , _derEvents :: [Event]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Events ::@ @[Event]@
--
describeEventsResponse :: DescribeEventsResponse
describeEventsResponse = DescribeEventsResponse
    { _derMarker = Nothing
    , _derEvents = mempty
    }

-- | An optional pagination token provided by a previous Events request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords .
derMarker :: Lens' DescribeEventsResponse (Maybe Text)
derMarker = lens _derMarker (\s a -> s { _derMarker = a })

-- | A list of Event instances.
derEvents :: Lens' DescribeEventsResponse [Event]
derEvents = lens _derEvents (\s a -> s { _derEvents = a })

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = RDS
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq & deMarker ?~ x)
        <$> (rs ^. derMarker)
