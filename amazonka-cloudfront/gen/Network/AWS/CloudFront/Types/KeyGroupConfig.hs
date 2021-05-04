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
-- Module      : Network.AWS.CloudFront.Types.KeyGroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key group configuration.
--
-- A key group contains a list of public keys that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies>.
--
-- /See:/ 'newKeyGroupConfig' smart constructor.
data KeyGroupConfig = KeyGroupConfig'
  { -- | A comment to describe the key group.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A name to identify the key group.
    name :: Prelude.Text,
    -- | A list of the identifiers of the public keys in the key group.
    items :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'keyGroupConfig_comment' - A comment to describe the key group.
--
-- 'name', 'keyGroupConfig_name' - A name to identify the key group.
--
-- 'items', 'keyGroupConfig_items' - A list of the identifiers of the public keys in the key group.
newKeyGroupConfig ::
  -- | 'name'
  Prelude.Text ->
  KeyGroupConfig
newKeyGroupConfig pName_ =
  KeyGroupConfig'
    { comment = Prelude.Nothing,
      name = pName_,
      items = Prelude.mempty
    }

-- | A comment to describe the key group.
keyGroupConfig_comment :: Lens.Lens' KeyGroupConfig (Prelude.Maybe Prelude.Text)
keyGroupConfig_comment = Lens.lens (\KeyGroupConfig' {comment} -> comment) (\s@KeyGroupConfig' {} a -> s {comment = a} :: KeyGroupConfig)

-- | A name to identify the key group.
keyGroupConfig_name :: Lens.Lens' KeyGroupConfig Prelude.Text
keyGroupConfig_name = Lens.lens (\KeyGroupConfig' {name} -> name) (\s@KeyGroupConfig' {} a -> s {name = a} :: KeyGroupConfig)

-- | A list of the identifiers of the public keys in the key group.
keyGroupConfig_items :: Lens.Lens' KeyGroupConfig [Prelude.Text]
keyGroupConfig_items = Lens.lens (\KeyGroupConfig' {items} -> items) (\s@KeyGroupConfig' {} a -> s {items = a} :: KeyGroupConfig) Prelude.. Prelude._Coerce

instance Prelude.FromXML KeyGroupConfig where
  parseXML x =
    KeyGroupConfig'
      Prelude.<$> (x Prelude..@? "Comment")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "PublicKey"
                  )

instance Prelude.Hashable KeyGroupConfig

instance Prelude.NFData KeyGroupConfig

instance Prelude.ToXML KeyGroupConfig where
  toXML KeyGroupConfig' {..} =
    Prelude.mconcat
      [ "Comment" Prelude.@= comment,
        "Name" Prelude.@= name,
        "Items"
          Prelude.@= Prelude.toXMLList "PublicKey" items
      ]
