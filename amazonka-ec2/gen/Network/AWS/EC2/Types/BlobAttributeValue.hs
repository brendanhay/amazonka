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
-- Module      : Network.AWS.EC2.Types.BlobAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BlobAttributeValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newBlobAttributeValue' smart constructor.
data BlobAttributeValue = BlobAttributeValue'
  { value :: Prelude.Maybe Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlobAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'blobAttributeValue_value' - Undocumented member.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newBlobAttributeValue ::
  BlobAttributeValue
newBlobAttributeValue =
  BlobAttributeValue' {value = Prelude.Nothing}

-- | Undocumented member.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
blobAttributeValue_value :: Lens.Lens' BlobAttributeValue (Prelude.Maybe Prelude.ByteString)
blobAttributeValue_value = Lens.lens (\BlobAttributeValue' {value} -> value) (\s@BlobAttributeValue' {} a -> s {value = a} :: BlobAttributeValue) Prelude.. Lens.mapping Prelude._Base64

instance Prelude.Hashable BlobAttributeValue

instance Prelude.NFData BlobAttributeValue

instance Prelude.ToQuery BlobAttributeValue where
  toQuery BlobAttributeValue' {..} =
    Prelude.mconcat ["Value" Prelude.=: value]
