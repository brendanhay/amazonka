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
-- Module      : Network.AWS.S3.Types.ErrorDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ErrorDocument where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | The error information.
--
-- /See:/ 'newErrorDocument' smart constructor.
data ErrorDocument = ErrorDocument'
  { -- | The object key name to use when a 4XX class error occurs.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ErrorDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'errorDocument_key' - The object key name to use when a 4XX class error occurs.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
newErrorDocument ::
  -- | 'key'
  ObjectKey ->
  ErrorDocument
newErrorDocument pKey_ = ErrorDocument' {key = pKey_}

-- | The object key name to use when a 4XX class error occurs.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
errorDocument_key :: Lens.Lens' ErrorDocument ObjectKey
errorDocument_key = Lens.lens (\ErrorDocument' {key} -> key) (\s@ErrorDocument' {} a -> s {key = a} :: ErrorDocument)

instance Prelude.FromXML ErrorDocument where
  parseXML x =
    ErrorDocument' Prelude.<$> (x Prelude..@ "Key")

instance Prelude.Hashable ErrorDocument

instance Prelude.NFData ErrorDocument

instance Prelude.ToXML ErrorDocument where
  toXML ErrorDocument' {..} =
    Prelude.mconcat ["Key" Prelude.@= key]
