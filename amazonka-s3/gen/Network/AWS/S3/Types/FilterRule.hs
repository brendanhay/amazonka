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
-- Module      : Network.AWS.S3.Types.FilterRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.FilterRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.FilterRuleName

-- | Specifies the Amazon S3 object key name to filter on and whether to
-- filter on the suffix or prefix of the key name.
--
-- /See:/ 'newFilterRule' smart constructor.
data FilterRule = FilterRule'
  { -- | The object key name prefix or suffix identifying one or more objects to
    -- which the filtering rule applies. The maximum length is 1,024
    -- characters. Overlapping prefixes and suffixes are not supported. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    name :: Prelude.Maybe FilterRuleName,
    -- | The value that the filter searches for in object key names.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filterRule_name' - The object key name prefix or suffix identifying one or more objects to
-- which the filtering rule applies. The maximum length is 1,024
-- characters. Overlapping prefixes and suffixes are not supported. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'value', 'filterRule_value' - The value that the filter searches for in object key names.
newFilterRule ::
  FilterRule
newFilterRule =
  FilterRule'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The object key name prefix or suffix identifying one or more objects to
-- which the filtering rule applies. The maximum length is 1,024
-- characters. Overlapping prefixes and suffixes are not supported. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the /Amazon Simple Storage Service Developer Guide/.
filterRule_name :: Lens.Lens' FilterRule (Prelude.Maybe FilterRuleName)
filterRule_name = Lens.lens (\FilterRule' {name} -> name) (\s@FilterRule' {} a -> s {name = a} :: FilterRule)

-- | The value that the filter searches for in object key names.
filterRule_value :: Lens.Lens' FilterRule (Prelude.Maybe Prelude.Text)
filterRule_value = Lens.lens (\FilterRule' {value} -> value) (\s@FilterRule' {} a -> s {value = a} :: FilterRule)

instance Prelude.FromXML FilterRule where
  parseXML x =
    FilterRule'
      Prelude.<$> (x Prelude..@? "Name")
      Prelude.<*> (x Prelude..@? "Value")

instance Prelude.Hashable FilterRule

instance Prelude.NFData FilterRule

instance Prelude.ToXML FilterRule where
  toXML FilterRule' {..} =
    Prelude.mconcat
      ["Name" Prelude.@= name, "Value" Prelude.@= value]
