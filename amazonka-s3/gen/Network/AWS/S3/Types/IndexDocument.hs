{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IndexDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IndexDocument where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container for the @Suffix@ element.
--
-- /See:/ 'newIndexDocument' smart constructor.
data IndexDocument = IndexDocument'
  { -- | A suffix that is appended to a request that is for a directory on the
    -- website endpoint (for example,if the suffix is index.html and you make a
    -- request to samplebucket\/images\/ the data that is returned will be for
    -- the object with the key name images\/index.html) The suffix must not be
    -- empty and must not include a slash character.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    suffix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IndexDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suffix', 'indexDocument_suffix' - A suffix that is appended to a request that is for a directory on the
-- website endpoint (for example,if the suffix is index.html and you make a
-- request to samplebucket\/images\/ the data that is returned will be for
-- the object with the key name images\/index.html) The suffix must not be
-- empty and must not include a slash character.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
newIndexDocument ::
  -- | 'suffix'
  Prelude.Text ->
  IndexDocument
newIndexDocument pSuffix_ =
  IndexDocument' {suffix = pSuffix_}

-- | A suffix that is appended to a request that is for a directory on the
-- website endpoint (for example,if the suffix is index.html and you make a
-- request to samplebucket\/images\/ the data that is returned will be for
-- the object with the key name images\/index.html) The suffix must not be
-- empty and must not include a slash character.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
indexDocument_suffix :: Lens.Lens' IndexDocument Prelude.Text
indexDocument_suffix = Lens.lens (\IndexDocument' {suffix} -> suffix) (\s@IndexDocument' {} a -> s {suffix = a} :: IndexDocument)

instance Prelude.FromXML IndexDocument where
  parseXML x =
    IndexDocument' Prelude.<$> (x Prelude..@ "Suffix")

instance Prelude.Hashable IndexDocument

instance Prelude.NFData IndexDocument

instance Prelude.ToXML IndexDocument where
  toXML IndexDocument' {..} =
    Prelude.mconcat ["Suffix" Prelude.@= suffix]
