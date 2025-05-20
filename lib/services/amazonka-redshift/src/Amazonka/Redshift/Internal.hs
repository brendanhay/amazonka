{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Amazonka.Redshift.Internal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Internal
  ( getAccountId,
    getCloudTrailAccountId,
  )
where

import Amazonka.Core
import Amazonka.Data

-- | This account identifier used to be used when attaching a policy
-- to your S3 bucket, allowing Redshift to upload and write database
-- audit logs.
--
-- This function should no longer be used, because Redshift now uses
-- service-principal credentials to deliver logs to S3.
--
-- /See:/ <https://docs.aws.amazon.com/redshift/latest/mgmt/db-auditing.html#db-auditing-bucket-permissions Bucket permissions for Amazon Redshift audit logging>.
getAccountId :: Region -> Maybe Text
getAccountId = \case
  NorthVirginia -> Just "193672423079"
  Ohio -> Just "391106570357"
  NorthCalifornia -> Just "262260360010"
  Oregon -> Just "902366379725"
  CapeTown -> Just "365689465814"
  HongKong -> Just "313564881002"
  Mumbai -> Just "865932855811"
  Osaka -> Just "090321488786"
  Seoul -> Just "760740231472"
  Singapore -> Just "361669875840"
  Sydney -> Just "762762565011"
  Tokyo -> Just "404641285394"
  Montreal -> Just "907379612154"
  Frankfurt -> Just "053454850223"
  Ireland -> Just "210876761215"
  London -> Just "307160386991"
  Milan -> Just "945612479654"
  Paris -> Just "915173422425"
  Stockholm -> Just "729911121831"
  Bahrain -> Just "013126148197"
  SaoPaulo -> Just "075028567923"
  Region' _ -> Nothing
{-# DEPRECATED
  getAccountId
  "Redshift now delivers logs using service-principal credentials. \
  \See the haddocks for more information."
  #-}

-- | This account identifier is used when Redshift calls other AWS
-- services for you, and may appear in your CloudTrail logs.
--
-- /See:/ <https://docs.aws.amazon.com/redshift/latest/mgmt/logging-with-cloudtrail.html#cloudtrail-rs-acct-ids Amazon Redshift account IDs in AWS CloudTrail logs>
-- /See:/ <https://docs.amazonaws.cn/en_us/redshift/latest/mgmt/logging-with-cloudtrail.html#cloudtrail-rs-acct-ids Amazon Redshift account IDs in AWS CloudTrail logs (includes China)>
getCloudTrailAccountId :: Region -> Maybe Text
getCloudTrailAccountId = \case
  NorthVirginia -> Just "368064434614"
  Ohio -> Just "790247189693"
  NorthCalifornia -> Just "703715109447"
  Oregon -> Just "473191095985"
  CapeTown -> Just "420376844563"
  HongKong -> Just "651179539253"
  Hyderabad -> Just "297058826802"
  Jakarta -> Just "623197973179"
  Malaysia -> Just "590184011157"
  Melbourne -> Just "945512339897"
  Mumbai -> Just "408097707231"
  MexicoCentral -> Just "058264411980"
  Osaka -> Just "398671365691"
  Seoul -> Just "713597048934"
  Singapore -> Just "960118270566"
  Sydney -> Just "485979073181"
  Tokyo -> Just "615915377779"
  Montreal -> Just "764870610256"
  Calgary -> Just "830903446466"
  Beijing -> Just "066403562008"
  Ningxia -> Just "194116488714"
  Frankfurt -> Just "434091160558"
  Ireland -> Just "246478207311"
  London -> Just "885798887673"
  Milan -> Just "041313461515"
  Paris -> Just "694668203235"
  Spain -> Just "028811157404"
  Stockholm -> Just "553461782468"
  Zurich -> Just "668912161003"
  TelAviv -> Just "901883065212"
  Bahrain -> Just "051362938876"
  UAE -> Just "595013617770"
  SaoPaulo -> Just "392442076723"
  Region' _ -> Nothing
