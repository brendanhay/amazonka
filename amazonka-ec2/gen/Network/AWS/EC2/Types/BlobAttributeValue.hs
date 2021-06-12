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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newBlobAttributeValue' smart constructor.
data BlobAttributeValue = BlobAttributeValue'
  { value :: Core.Maybe Core.Base64
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  BlobAttributeValue' {value = Core.Nothing}

-- | Undocumented member.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
blobAttributeValue_value :: Lens.Lens' BlobAttributeValue (Core.Maybe Core.ByteString)
blobAttributeValue_value = Lens.lens (\BlobAttributeValue' {value} -> value) (\s@BlobAttributeValue' {} a -> s {value = a} :: BlobAttributeValue) Core.. Lens.mapping Core._Base64

instance Core.Hashable BlobAttributeValue

instance Core.NFData BlobAttributeValue

instance Core.ToQuery BlobAttributeValue where
  toQuery BlobAttributeValue' {..} =
    Core.mconcat ["Value" Core.=: value]
