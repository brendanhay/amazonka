{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Amazonka.Redshift.Internal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Internal
  ( getAccountId,
  )
where

import Amazonka.Core
import Amazonka.Data

-- | This account identifier is used when attaching a policy to your S3 bucket
-- allowing Redshift to upload and write database audit logs.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/mgmt/db-auditing.html#db-auditing-enable-logging Enabling Database Audit Logging>.
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
  _other -> Nothing
