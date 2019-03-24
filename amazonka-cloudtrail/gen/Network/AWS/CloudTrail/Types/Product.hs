{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Product where

import Network.AWS.CloudTrail.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon S3 buckets or AWS Lambda functions that you specify in your event selectors for your trail to log data events. Data events provide insight into the resource operations performed on or within a resource itself. These are also known as data plane operations. You can specify up to 250 data resources for a trail.
--
--
-- The following example demonstrates how logging works when you configure logging of all data events for an S3 bucket named @bucket-1@ . In this example, the CloudTrail user spcified an empty prefix, and the option to log both @Read@ and @Write@ data events.
--
--     * A user uploads an image file to @bucket-1@ .
--
--     * The @PutObject@ API operation is an Amazon S3 object-level API. It is recorded as a data event in CloudTrail. Because the CloudTrail user specified an S3 bucket with an empty prefix, events that occur on any object in that bucket are logged. The trail processes and logs the event.
--
--     * A user uploads an object to an Amazon S3 bucket named @arn:aws:s3:::bucket-2@ .
--
--     * The @PutObject@ API operation occurred for an object in an S3 bucket that the CloudTrail user didn't specify for the trail. The trail doesn
