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
-- Module      : Amazonka.CloudFront.Types.KeyGroupConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.KeyGroupConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key group configuration.
--
-- A key group contains a list of public keys that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies>.
--
-- /See:/ 'newKeyGroupConfig' smart constructor.
data KeyGroupConfig = KeyGroupConfig'
  { -- | A comment to describe the key group. The comment cannot be longer than
    -- 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A name to identify the key group.
    name :: Prelude.Text,
    -- | A list of the identifiers of the public keys in the key group.
    items :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'keyGroupConfig_comment' - A comment to describe the key group. The comment cannot be longer than
-- 128 characters.
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

-- | A comment to describe the key group. The comment cannot be longer than
-- 128 characters.
keyGroupConfig_comment :: Lens.Lens' KeyGroupConfig (Prelude.Maybe Prelude.Text)
keyGroupConfig_comment = Lens.lens (\KeyGroupConfig' {comment} -> comment) (\s@KeyGroupConfig' {} a -> s {comment = a} :: KeyGroupConfig)

-- | A name to identify the key group.
keyGroupConfig_name :: Lens.Lens' KeyGroupConfig Prelude.Text
keyGroupConfig_name = Lens.lens (\KeyGroupConfig' {name} -> name) (\s@KeyGroupConfig' {} a -> s {name = a} :: KeyGroupConfig)

-- | A list of the identifiers of the public keys in the key group.
keyGroupConfig_items :: Lens.Lens' KeyGroupConfig [Prelude.Text]
keyGroupConfig_items = Lens.lens (\KeyGroupConfig' {items} -> items) (\s@KeyGroupConfig' {} a -> s {items = a} :: KeyGroupConfig) Prelude.. Lens.coerced

instance Data.FromXML KeyGroupConfig where
  parseXML x =
    KeyGroupConfig'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "PublicKey"
                  )

instance Prelude.Hashable KeyGroupConfig where
  hashWithSalt _salt KeyGroupConfig' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` items

instance Prelude.NFData KeyGroupConfig where
  rnf KeyGroupConfig' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf items

instance Data.ToXML KeyGroupConfig where
  toXML KeyGroupConfig' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "Name" Data.@= name,
        "Items" Data.@= Data.toXMLList "PublicKey" items
      ]
