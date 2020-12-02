{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CompletedPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CompletedPart where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Details of the parts that were uploaded.
--
--
--
-- /See:/ 'completedPart' smart constructor.
data CompletedPart = CompletedPart'
  { _cpPartNumber :: !Int,
    _cpETag :: !ETag
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompletedPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPartNumber' - Part number that identifies the part. This is a positive integer between 1 and 10,000.
--
-- * 'cpETag' - Entity tag returned when the part was uploaded.
completedPart ::
  -- | 'cpPartNumber'
  Int ->
  -- | 'cpETag'
  ETag ->
  CompletedPart
completedPart pPartNumber_ pETag_ =
  CompletedPart' {_cpPartNumber = pPartNumber_, _cpETag = pETag_}

-- | Part number that identifies the part. This is a positive integer between 1 and 10,000.
cpPartNumber :: Lens' CompletedPart Int
cpPartNumber = lens _cpPartNumber (\s a -> s {_cpPartNumber = a})

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart ETag
cpETag = lens _cpETag (\s a -> s {_cpETag = a})

instance Hashable CompletedPart

instance NFData CompletedPart

instance ToXML CompletedPart where
  toXML CompletedPart' {..} =
    mconcat ["PartNumber" @= _cpPartNumber, "ETag" @= _cpETag]
