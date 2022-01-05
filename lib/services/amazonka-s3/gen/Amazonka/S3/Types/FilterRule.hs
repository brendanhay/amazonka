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
-- Module      : Amazonka.S3.Types.FilterRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.FilterRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.FilterRuleName

-- | Specifies the Amazon S3 object key name to filter on and whether to
-- filter on the suffix or prefix of the key name.
--
-- /See:/ 'newFilterRule' smart constructor.
data FilterRule = FilterRule'
  { -- | The value that the filter searches for in object key names.
    value :: Prelude.Maybe Prelude.Text,
    -- | The object key name prefix or suffix identifying one or more objects to
    -- which the filtering rule applies. The maximum length is 1,024
    -- characters. Overlapping prefixes and suffixes are not supported. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
    -- in the /Amazon S3 User Guide/.
    name :: Prelude.Maybe FilterRuleName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'filterRule_value' - The value that the filter searches for in object key names.
--
-- 'name', 'filterRule_name' - The object key name prefix or suffix identifying one or more objects to
-- which the filtering rule applies. The maximum length is 1,024
-- characters. Overlapping prefixes and suffixes are not supported. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the /Amazon S3 User Guide/.
newFilterRule ::
  FilterRule
newFilterRule =
  FilterRule'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value that the filter searches for in object key names.
filterRule_value :: Lens.Lens' FilterRule (Prelude.Maybe Prelude.Text)
filterRule_value = Lens.lens (\FilterRule' {value} -> value) (\s@FilterRule' {} a -> s {value = a} :: FilterRule)

-- | The object key name prefix or suffix identifying one or more objects to
-- which the filtering rule applies. The maximum length is 1,024
-- characters. Overlapping prefixes and suffixes are not supported. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the /Amazon S3 User Guide/.
filterRule_name :: Lens.Lens' FilterRule (Prelude.Maybe FilterRuleName)
filterRule_name = Lens.lens (\FilterRule' {name} -> name) (\s@FilterRule' {} a -> s {name = a} :: FilterRule)

instance Core.FromXML FilterRule where
  parseXML x =
    FilterRule'
      Prelude.<$> (x Core..@? "Value") Prelude.<*> (x Core..@? "Name")

instance Prelude.Hashable FilterRule where
  hashWithSalt _salt FilterRule' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData FilterRule where
  rnf FilterRule' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToXML FilterRule where
  toXML FilterRule' {..} =
    Prelude.mconcat
      ["Value" Core.@= value, "Name" Core.@= name]
