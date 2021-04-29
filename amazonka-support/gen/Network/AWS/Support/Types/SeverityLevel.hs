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
-- Module      : Network.AWS.Support.Types.SeverityLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.SeverityLevel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A code and name pair that represents the severity level of a support
-- case. The available values depend on the support plan for the account.
-- For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
-- in the /AWS Support User Guide/.
--
-- /See:/ 'newSeverityLevel' smart constructor.
data SeverityLevel = SeverityLevel'
  { -- | The code for case severity level.
    --
    -- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
    code :: Prelude.Maybe Prelude.Text,
    -- | The name of the severity level that corresponds to the severity level
    -- code.
    --
    -- The values returned by the API differ from the values that are displayed
    -- in the AWS Support Center. For example, for the code \"low\", the API
    -- name is \"Low\", but the name in the Support Center is \"General
    -- guidance\". These are the Support Center code\/name mappings:
    --
    -- -   @low@: General guidance
    --
    -- -   @normal@: System impaired
    --
    -- -   @high@: Production system impaired
    --
    -- -   @urgent@: Production system down
    --
    -- -   @critical@: Business-critical system down
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
    -- in the /AWS Support User Guide/.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SeverityLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'severityLevel_code' - The code for case severity level.
--
-- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
--
-- 'name', 'severityLevel_name' - The name of the severity level that corresponds to the severity level
-- code.
--
-- The values returned by the API differ from the values that are displayed
-- in the AWS Support Center. For example, for the code \"low\", the API
-- name is \"Low\", but the name in the Support Center is \"General
-- guidance\". These are the Support Center code\/name mappings:
--
-- -   @low@: General guidance
--
-- -   @normal@: System impaired
--
-- -   @high@: Production system impaired
--
-- -   @urgent@: Production system down
--
-- -   @critical@: Business-critical system down
--
-- For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
-- in the /AWS Support User Guide/.
newSeverityLevel ::
  SeverityLevel
newSeverityLevel =
  SeverityLevel'
    { code = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The code for case severity level.
--
-- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
severityLevel_code :: Lens.Lens' SeverityLevel (Prelude.Maybe Prelude.Text)
severityLevel_code = Lens.lens (\SeverityLevel' {code} -> code) (\s@SeverityLevel' {} a -> s {code = a} :: SeverityLevel)

-- | The name of the severity level that corresponds to the severity level
-- code.
--
-- The values returned by the API differ from the values that are displayed
-- in the AWS Support Center. For example, for the code \"low\", the API
-- name is \"Low\", but the name in the Support Center is \"General
-- guidance\". These are the Support Center code\/name mappings:
--
-- -   @low@: General guidance
--
-- -   @normal@: System impaired
--
-- -   @high@: Production system impaired
--
-- -   @urgent@: Production system down
--
-- -   @critical@: Business-critical system down
--
-- For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
-- in the /AWS Support User Guide/.
severityLevel_name :: Lens.Lens' SeverityLevel (Prelude.Maybe Prelude.Text)
severityLevel_name = Lens.lens (\SeverityLevel' {name} -> name) (\s@SeverityLevel' {} a -> s {name = a} :: SeverityLevel)

instance Prelude.FromJSON SeverityLevel where
  parseJSON =
    Prelude.withObject
      "SeverityLevel"
      ( \x ->
          SeverityLevel'
            Prelude.<$> (x Prelude..:? "code")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable SeverityLevel

instance Prelude.NFData SeverityLevel
