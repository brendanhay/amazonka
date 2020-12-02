{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Invalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Invalidation where

import Network.AWS.CloudFront.Types.InvalidationBatch
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An invalidation.
--
--
--
-- /See:/ 'invalidation' smart constructor.
data Invalidation = Invalidation'
  { _iId :: !Text,
    _iStatus :: !Text,
    _iCreateTime :: !ISO8601,
    _iInvalidationBatch :: !InvalidationBatch
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Invalidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iId' - The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
--
-- * 'iStatus' - The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
--
-- * 'iCreateTime' - The date and time the invalidation request was first made.
--
-- * 'iInvalidationBatch' - The current invalidation information for the batch request.
invalidation ::
  -- | 'iId'
  Text ->
  -- | 'iStatus'
  Text ->
  -- | 'iCreateTime'
  UTCTime ->
  -- | 'iInvalidationBatch'
  InvalidationBatch ->
  Invalidation
invalidation pId_ pStatus_ pCreateTime_ pInvalidationBatch_ =
  Invalidation'
    { _iId = pId_,
      _iStatus = pStatus_,
      _iCreateTime = _Time # pCreateTime_,
      _iInvalidationBatch = pInvalidationBatch_
    }

-- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
iId :: Lens' Invalidation Text
iId = lens _iId (\s a -> s {_iId = a})

-- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
iStatus :: Lens' Invalidation Text
iStatus = lens _iStatus (\s a -> s {_iStatus = a})

-- | The date and time the invalidation request was first made.
iCreateTime :: Lens' Invalidation UTCTime
iCreateTime = lens _iCreateTime (\s a -> s {_iCreateTime = a}) . _Time

-- | The current invalidation information for the batch request.
iInvalidationBatch :: Lens' Invalidation InvalidationBatch
iInvalidationBatch = lens _iInvalidationBatch (\s a -> s {_iInvalidationBatch = a})

instance FromXML Invalidation where
  parseXML x =
    Invalidation'
      <$> (x .@ "Id")
      <*> (x .@ "Status")
      <*> (x .@ "CreateTime")
      <*> (x .@ "InvalidationBatch")

instance Hashable Invalidation

instance NFData Invalidation
