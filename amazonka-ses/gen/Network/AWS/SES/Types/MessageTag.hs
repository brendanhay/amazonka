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
-- Module      : Network.AWS.SES.Types.MessageTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.MessageTag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the name and value of a tag that you can provide to @SendEmail@
-- or @SendRawEmail@ to apply to an email.
--
-- Message tags, which you use with configuration sets, enable you to
-- publish email sending events. For information about using configuration
-- sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newMessageTag' smart constructor.
data MessageTag = MessageTag'
  { -- | The name of the tag. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Contain less than 256 characters.
    name :: Prelude.Text,
    -- | The value of the tag. The value must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Contain less than 256 characters.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MessageTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'messageTag_name' - The name of the tag. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
--
-- 'value', 'messageTag_value' - The value of the tag. The value must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
newMessageTag ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  MessageTag
newMessageTag pName_ pValue_ =
  MessageTag' {name = pName_, value = pValue_}

-- | The name of the tag. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
messageTag_name :: Lens.Lens' MessageTag Prelude.Text
messageTag_name = Lens.lens (\MessageTag' {name} -> name) (\s@MessageTag' {} a -> s {name = a} :: MessageTag)

-- | The value of the tag. The value must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
messageTag_value :: Lens.Lens' MessageTag Prelude.Text
messageTag_value = Lens.lens (\MessageTag' {value} -> value) (\s@MessageTag' {} a -> s {value = a} :: MessageTag)

instance Prelude.Hashable MessageTag

instance Prelude.NFData MessageTag

instance Prelude.ToQuery MessageTag where
  toQuery MessageTag' {..} =
    Prelude.mconcat
      ["Name" Prelude.=: name, "Value" Prelude.=: value]
