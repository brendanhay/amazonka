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
-- Module      : Amazonka.SESV2.Types.MessageTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.MessageTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the name and value of a tag that you apply to an email. You can
-- use message tags when you publish email sending events.
--
-- /See:/ 'newMessageTag' smart constructor.
data MessageTag = MessageTag'
  { -- | The name of the message tag. The message tag name has to meet the
    -- following criteria:
    --
    -- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
    --     underscores (_), or dashes (-).
    --
    -- -   It can contain no more than 256 characters.
    name :: Prelude.Text,
    -- | The value of the message tag. The message tag value has to meet the
    -- following criteria:
    --
    -- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
    --     underscores (_), or dashes (-).
    --
    -- -   It can contain no more than 256 characters.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'messageTag_name' - The name of the message tag. The message tag name has to meet the
-- following criteria:
--
-- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-).
--
-- -   It can contain no more than 256 characters.
--
-- 'value', 'messageTag_value' - The value of the message tag. The message tag value has to meet the
-- following criteria:
--
-- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-).
--
-- -   It can contain no more than 256 characters.
newMessageTag ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  MessageTag
newMessageTag pName_ pValue_ =
  MessageTag' {name = pName_, value = pValue_}

-- | The name of the message tag. The message tag name has to meet the
-- following criteria:
--
-- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-).
--
-- -   It can contain no more than 256 characters.
messageTag_name :: Lens.Lens' MessageTag Prelude.Text
messageTag_name = Lens.lens (\MessageTag' {name} -> name) (\s@MessageTag' {} a -> s {name = a} :: MessageTag)

-- | The value of the message tag. The message tag value has to meet the
-- following criteria:
--
-- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-).
--
-- -   It can contain no more than 256 characters.
messageTag_value :: Lens.Lens' MessageTag Prelude.Text
messageTag_value = Lens.lens (\MessageTag' {value} -> value) (\s@MessageTag' {} a -> s {value = a} :: MessageTag)

instance Prelude.Hashable MessageTag where
  hashWithSalt _salt MessageTag' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData MessageTag where
  rnf MessageTag' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON MessageTag where
  toJSON MessageTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
