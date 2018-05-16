{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Network.AWS.Redshift.Internal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Internal
    ( getAccountId
    ) where

import Network.AWS.Prelude

-- | This account identifier is used when attaching a policy to your S3 bucket
-- allowing Redshift to upload and write database audit logs.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/mgmt/db-auditing.html#db-auditing-enable-logging Enabling Database Audit Logging>.
getAccountId :: Region -> Maybe Text
getAccountId = \case
    NorthVirginia   -> Just "193672423079"
    Ohio            -> Just "391106570357"
    NorthCalifornia -> Just "262260360010"
    Oregon          -> Just "902366379725"
    Montreal        -> Just "907379612154"
    Tokyo           -> Just "404641285394"
    Seoul           -> Just "760740231472"
    Mumbai          -> Just "865932855811"
    Singapore       -> Just "361669875840"
    Sydney          -> Just "762762565011"
    SaoPaulo        -> Just "075028567923"
    Frankfurt       -> Just "053454850223"
    Ireland         -> Just "210876761215"
    London          -> Just "307160386991"
    GovCloud        -> Nothing
    GovCloudFIPS    -> Nothing
    Beijing         -> Nothing
