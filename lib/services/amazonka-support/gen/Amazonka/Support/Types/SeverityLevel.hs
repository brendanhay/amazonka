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
-- Module      : Amazonka.Support.Types.SeverityLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.SeverityLevel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A code and name pair that represents the severity level of a support
-- case. The available values depend on the support plan for the account.
-- For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
-- in the /Amazon Web Services Support User Guide/.
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
    -- The values returned by the API are different from the values that appear
    -- in the Amazon Web Services Support Center. For example, the API uses the
    -- code @low@, but the name appears as General guidance in Support Center.
    --
    -- The following are the API code names and how they appear in the console:
    --
    -- -   @low@ - General guidance
    --
    -- -   @normal@ - System impaired
    --
    -- -   @high@ - Production system impaired
    --
    -- -   @urgent@ - Production system down
    --
    -- -   @critical@ - Business-critical system down
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
    -- in the /Amazon Web Services Support User Guide/.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- The values returned by the API are different from the values that appear
-- in the Amazon Web Services Support Center. For example, the API uses the
-- code @low@, but the name appears as General guidance in Support Center.
--
-- The following are the API code names and how they appear in the console:
--
-- -   @low@ - General guidance
--
-- -   @normal@ - System impaired
--
-- -   @high@ - Production system impaired
--
-- -   @urgent@ - Production system down
--
-- -   @critical@ - Business-critical system down
--
-- For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
-- in the /Amazon Web Services Support User Guide/.
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
-- The values returned by the API are different from the values that appear
-- in the Amazon Web Services Support Center. For example, the API uses the
-- code @low@, but the name appears as General guidance in Support Center.
--
-- The following are the API code names and how they appear in the console:
--
-- -   @low@ - General guidance
--
-- -   @normal@ - System impaired
--
-- -   @high@ - Production system impaired
--
-- -   @urgent@ - Production system down
--
-- -   @critical@ - Business-critical system down
--
-- For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity>
-- in the /Amazon Web Services Support User Guide/.
severityLevel_name :: Lens.Lens' SeverityLevel (Prelude.Maybe Prelude.Text)
severityLevel_name = Lens.lens (\SeverityLevel' {name} -> name) (\s@SeverityLevel' {} a -> s {name = a} :: SeverityLevel)

instance Data.FromJSON SeverityLevel where
  parseJSON =
    Data.withObject
      "SeverityLevel"
      ( \x ->
          SeverityLevel'
            Prelude.<$> (x Data..:? "code") Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable SeverityLevel where
  hashWithSalt _salt SeverityLevel' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` name

instance Prelude.NFData SeverityLevel where
  rnf SeverityLevel' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf name
