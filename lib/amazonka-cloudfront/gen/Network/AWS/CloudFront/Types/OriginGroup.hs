{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroup where

import Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
import Network.AWS.CloudFront.Types.OriginGroupMembers
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An origin group includes two origins (a primary origin and a second origin to failover to) and a failover criteria that you specify. You create an origin group to support origin failover in CloudFront. When you create or update a distribution, you can specifiy the origin group instead of a single origin, and CloudFront will failover from the primary origin to the second origin under the failover conditions that you've chosen.
--
--
--
-- /See:/ 'originGroup' smart constructor.
data OriginGroup = OriginGroup'
  { _ogId :: !Text,
    _ogFailoverCriteria :: !OriginGroupFailoverCriteria,
    _ogMembers :: !OriginGroupMembers
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogId' - The origin group's ID.
--
-- * 'ogFailoverCriteria' - A complex type that contains information about the failover criteria for an origin group.
--
-- * 'ogMembers' - A complex type that contains information about the origins in an origin group.
originGroup ::
  -- | 'ogId'
  Text ->
  -- | 'ogFailoverCriteria'
  OriginGroupFailoverCriteria ->
  -- | 'ogMembers'
  OriginGroupMembers ->
  OriginGroup
originGroup pId_ pFailoverCriteria_ pMembers_ =
  OriginGroup'
    { _ogId = pId_,
      _ogFailoverCriteria = pFailoverCriteria_,
      _ogMembers = pMembers_
    }

-- | The origin group's ID.
ogId :: Lens' OriginGroup Text
ogId = lens _ogId (\s a -> s {_ogId = a})

-- | A complex type that contains information about the failover criteria for an origin group.
ogFailoverCriteria :: Lens' OriginGroup OriginGroupFailoverCriteria
ogFailoverCriteria = lens _ogFailoverCriteria (\s a -> s {_ogFailoverCriteria = a})

-- | A complex type that contains information about the origins in an origin group.
ogMembers :: Lens' OriginGroup OriginGroupMembers
ogMembers = lens _ogMembers (\s a -> s {_ogMembers = a})

instance FromXML OriginGroup where
  parseXML x =
    OriginGroup'
      <$> (x .@ "Id") <*> (x .@ "FailoverCriteria") <*> (x .@ "Members")

instance Hashable OriginGroup

instance NFData OriginGroup

instance ToXML OriginGroup where
  toXML OriginGroup' {..} =
    mconcat
      [ "Id" @= _ogId,
        "FailoverCriteria" @= _ogFailoverCriteria,
        "Members" @= _ogMembers
      ]
