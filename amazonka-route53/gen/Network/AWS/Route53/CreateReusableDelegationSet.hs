{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones. If a hosted zoned ID is specified, @CreateReusableDelegationSet@ marks the delegation set associated with that zone as reusable.
--
--
-- For information about using a reusable delegation set to configure white label name servers, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html Configuring White Label Name Servers> .
--
-- The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:
--
--     * Create a reusable delegation set.
--
--     * Recreate hosted zones, and reduce the TTL to 60 seconds or less.
--
--     * Recreate resource record sets in the new hosted zones.
--
--     * Change the registrar's name servers to use the name servers for the new hosted zones.
--
--     * Monitor traffic for the website or application.
--
--     * Change TTLs back to their original values.
--
--
--
-- If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:
--
--     * For small numbers of hosted zones
