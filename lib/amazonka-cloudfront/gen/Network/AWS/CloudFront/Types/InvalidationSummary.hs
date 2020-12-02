{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of an invalidation request.
--
--
--
-- /See:/ 'invalidationSummary' smart constructor.
data InvalidationSummary = InvalidationSummary'
  { _isId :: !Text,
    _isCreateTime :: !ISO8601,
    _isStatus :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InvalidationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isId' - The unique ID for an invalidation request.
--
-- * 'isCreateTime' - The time that an invalidation request was created.
--
-- * 'isStatus' - The status of an invalidation request.
invalidationSummary ::
  -- | 'isId'
  Text ->
  -- | 'isCreateTime'
  UTCTime ->
  -- | 'isStatus'
  Text ->
  InvalidationSummary
invalidationSummary pId_ pCreateTime_ pStatus_ =
  InvalidationSummary'
    { _isId = pId_,
      _isCreateTime = _Time # pCreateTime_,
      _isStatus = pStatus_
    }

-- | The unique ID for an invalidation request.
isId :: Lens' InvalidationSummary Text
isId = lens _isId (\s a -> s {_isId = a})

-- | The time that an invalidation request was created.
isCreateTime :: Lens' InvalidationSummary UTCTime
isCreateTime = lens _isCreateTime (\s a -> s {_isCreateTime = a}) . _Time

-- | The status of an invalidation request.
isStatus :: Lens' InvalidationSummary Text
isStatus = lens _isStatus (\s a -> s {_isStatus = a})

instance FromXML InvalidationSummary where
  parseXML x =
    InvalidationSummary'
      <$> (x .@ "Id") <*> (x .@ "CreateTime") <*> (x .@ "Status")

instance Hashable InvalidationSummary

instance NFData InvalidationSummary
