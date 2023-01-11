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
-- Module      : Amazonka.S3.Types.IndexDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.IndexDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML IndexDocument where
  parseXML x =
    IndexDocument' Prelude.<$> (x Data..@ "Suffix")

instance Prelude.Hashable IndexDocument where
  hashWithSalt _salt IndexDocument' {..} =
    _salt `Prelude.hashWithSalt` suffix

instance Prelude.NFData IndexDocument where
  rnf IndexDocument' {..} = Prelude.rnf suffix

instance Data.ToXML IndexDocument where
  toXML IndexDocument' {..} =
    Prelude.mconcat ["Suffix" Data.@= suffix]
