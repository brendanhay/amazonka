{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeEvents
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
module Network.AWS.RDS.V2013_09_09.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , mkDescribeEventsMessage
    -- ** Request lenses
    , demSourceIdentifier
    , demSourceType
    , demStartTime
    , demEndTime
    , demDuration
    , demEventCategories
    , demMaxRecords
    , demMarker

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , emMarker
    , emEvents
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEvents' request.
mkDescribeEventsMessage :: DescribeEvents
mkDescribeEventsMessage = DescribeEvents
    { _demSourceIdentifier = Nothing
    , _demSourceType = Nothing
    , _demStartTime = Nothing
    , _demEndTime = Nothing
    , _demDuration = Nothing
    , _demEventCategories = mempty
    , _demMaxRecords = Nothing
    , _demMarker = Nothing
    }
{-# INLINE mkDescribeEventsMessage #-}

data DescribeEvents = DescribeEvents
    { _demSourceIdentifier :: Maybe Text
      -- ^ The identifier of the event source for which events will be
      -- returned. If not specified, then all sources are included in the
      -- response. Constraints: If SourceIdentifier is supplied,
      -- SourceType must also be provided. If the source type is
      -- DBInstance, then a DBInstanceIdentifier must be supplied. If the
      -- source type is DBSecurityGroup, a DBSecurityGroupName must be
      -- supplied. If the source type is DBParameterGroup, a
      -- DBParameterGroupName must be supplied. If the source type is
      -- DBSnapshot, a DBSnapshotIdentifier must be supplied. Cannot end
      -- with a hyphen or contain two consecutive hyphens.
    , _demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is
      -- specified, all events are returned.
    , _demStartTime :: Maybe ISO8601
      -- ^ The beginning of the time interval to retrieve events for,
      -- specified in ISO 8601 format. For more information about ISO
      -- 8601, go to the ISO8601 Wikipedia page. Example:
      -- 2009-07-08T18:00Z.
    , _demEndTime :: Maybe ISO8601
      -- ^ The end of the time interval for which to retrieve events,
      -- specified in ISO 8601 format. For more information about ISO
      -- 8601, go to the ISO8601 Wikipedia page. Example:
      -- 2009-07-08T18:00Z.
    , _demDuration :: Maybe Integer
      -- ^ The number of minutes to retrieve events for. Default: 60.
    , _demEventCategories :: [Text]
      -- ^ A list of event categories that trigger notifications for a event
      -- notification subscription.
    , _demMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _demMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeEvents request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    } deriving (Show, Generic)

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response. Constraints:
-- If SourceIdentifier is supplied, SourceType must also be provided. If the
-- source type is DBInstance, then a DBInstanceIdentifier must be supplied. If
-- the source type is DBSecurityGroup, a DBSecurityGroupName must be supplied.
-- If the source type is DBParameterGroup, a DBParameterGroupName must be
-- supplied. If the source type is DBSnapshot, a DBSnapshotIdentifier must be
-- supplied. Cannot end with a hyphen or contain two consecutive hyphens.
demSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
demSourceIdentifier = lens _demSourceIdentifier (\s a -> s { _demSourceIdentifier = a })
{-# INLINE demSourceIdentifier #-}

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
demSourceType :: Lens' DescribeEvents (Maybe SourceType)
demSourceType = lens _demSourceType (\s a -> s { _demSourceType = a })
{-# INLINE demSourceType #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demStartTime :: Lens' DescribeEvents (Maybe ISO8601)
demStartTime = lens _demStartTime (\s a -> s { _demStartTime = a })
{-# INLINE demStartTime #-}

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demEndTime :: Lens' DescribeEvents (Maybe ISO8601)
demEndTime = lens _demEndTime (\s a -> s { _demEndTime = a })
{-# INLINE demEndTime #-}

-- | The number of minutes to retrieve events for. Default: 60.
demDuration :: Lens' DescribeEvents (Maybe Integer)
demDuration = lens _demDuration (\s a -> s { _demDuration = a })
{-# INLINE demDuration #-}

-- | A list of event categories that trigger notifications for a event
-- notification subscription.
demEventCategories :: Lens' DescribeEvents ([Text])
demEventCategories = lens _demEventCategories (\s a -> s { _demEventCategories = a })
{-# INLINE demEventCategories #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
demMaxRecords :: Lens' DescribeEvents (Maybe Integer)
demMaxRecords = lens _demMaxRecords (\s a -> s { _demMaxRecords = a })
{-# INLINE demMaxRecords #-}

-- | An optional pagination token provided by a previous DescribeEvents request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords.
demMarker :: Lens' DescribeEvents (Maybe Text)
demMarker = lens _demMarker (\s a -> s { _demMarker = a })
{-# INLINE demMarker #-}

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _emMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous Events
      -- request. If this parameter is specified, the response includes
      -- only records beyond the marker, up to the value specified by
      -- MaxRecords .
    , _emEvents :: [Event]
      -- ^ A list of Event instances.
    } deriving (Show, Generic)

-- | An optional pagination token provided by a previous Events request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords .
emMarker :: Lens' DescribeEventsResponse (Maybe Text)
emMarker = lens _emMarker (\s a -> s { _emMarker = a })
{-# INLINE emMarker #-}

-- | A list of Event instances.
emEvents :: Lens' DescribeEventsResponse ([Event])
emEvents = lens _emEvents (\s a -> s { _emEvents = a })
{-# INLINE emEvents #-}

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = RDS
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq { _demMarker = Just x })
        <$> (_emMarker rs)
