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
    , describeEvents
    -- ** Request lenses
    , demEventCategories
    , demDuration
    , demMaxRecords
    , demSourceType
    , demSourceIdentifier
    , demMarker
    , demStartTime
    , demEndTime

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , emEvents
    , emMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEvents' request.
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _demEventCategories = mempty
    , _demDuration = Nothing
    , _demMaxRecords = Nothing
    , _demSourceType = Nothing
    , _demSourceIdentifier = Nothing
    , _demMarker = Nothing
    , _demStartTime = Nothing
    , _demEndTime = Nothing
    }
{-# INLINE describeEvents #-}

data DescribeEvents = DescribeEvents
    { _demEventCategories :: [Text]
      -- ^ A list of event categories that trigger notifications for a event
      -- notification subscription.
    , _demDuration :: Maybe Integer
      -- ^ The number of minutes to retrieve events for. Default: 60.
    , _demMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is
      -- specified, all events are returned.
    , _demSourceIdentifier :: Maybe Text
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
    , _demMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeEvents request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
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
    } deriving (Show, Generic)

-- | A list of event categories that trigger notifications for a event
-- notification subscription.
demEventCategories :: Lens' DescribeEvents ([Text])
demEventCategories f x =
    f (_demEventCategories x)
        <&> \y -> x { _demEventCategories = y }
{-# INLINE demEventCategories #-}

-- | The number of minutes to retrieve events for. Default: 60.
demDuration :: Lens' DescribeEvents (Maybe Integer)
demDuration f x =
    f (_demDuration x)
        <&> \y -> x { _demDuration = y }
{-# INLINE demDuration #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
demMaxRecords :: Lens' DescribeEvents (Maybe Integer)
demMaxRecords f x =
    f (_demMaxRecords x)
        <&> \y -> x { _demMaxRecords = y }
{-# INLINE demMaxRecords #-}

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
demSourceType :: Lens' DescribeEvents (Maybe SourceType)
demSourceType f x =
    f (_demSourceType x)
        <&> \y -> x { _demSourceType = y }
{-# INLINE demSourceType #-}

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response. Constraints:
-- If SourceIdentifier is supplied, SourceType must also be provided. If the
-- source type is DBInstance, then a DBInstanceIdentifier must be supplied. If
-- the source type is DBSecurityGroup, a DBSecurityGroupName must be supplied.
-- If the source type is DBParameterGroup, a DBParameterGroupName must be
-- supplied. If the source type is DBSnapshot, a DBSnapshotIdentifier must be
-- supplied. Cannot end with a hyphen or contain two consecutive hyphens.
demSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
demSourceIdentifier f x =
    f (_demSourceIdentifier x)
        <&> \y -> x { _demSourceIdentifier = y }
{-# INLINE demSourceIdentifier #-}

-- | An optional pagination token provided by a previous DescribeEvents request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords.
demMarker :: Lens' DescribeEvents (Maybe Text)
demMarker f x =
    f (_demMarker x)
        <&> \y -> x { _demMarker = y }
{-# INLINE demMarker #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demStartTime :: Lens' DescribeEvents (Maybe ISO8601)
demStartTime f x =
    f (_demStartTime x)
        <&> \y -> x { _demStartTime = y }
{-# INLINE demStartTime #-}

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demEndTime :: Lens' DescribeEvents (Maybe ISO8601)
demEndTime f x =
    f (_demEndTime x)
        <&> \y -> x { _demEndTime = y }
{-# INLINE demEndTime #-}

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _emEvents :: [Event]
      -- ^ A list of Event instances.
    , _emMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous Events
      -- request. If this parameter is specified, the response includes
      -- only records beyond the marker, up to the value specified by
      -- MaxRecords .
    } deriving (Show, Generic)

-- | A list of Event instances.
emEvents :: Lens' DescribeEventsResponse ([Event])
emEvents f x =
    f (_emEvents x)
        <&> \y -> x { _emEvents = y }
{-# INLINE emEvents #-}

-- | An optional pagination token provided by a previous Events request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords .
emMarker :: Lens' DescribeEventsResponse (Maybe Text)
emMarker f x =
    f (_emMarker x)
        <&> \y -> x { _emMarker = y }
{-# INLINE emMarker #-}

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
