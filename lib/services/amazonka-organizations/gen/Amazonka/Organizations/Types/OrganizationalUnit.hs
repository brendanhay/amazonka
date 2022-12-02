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
-- Module      : Amazonka.Organizations.Types.OrganizationalUnit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.OrganizationalUnit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an organizational unit (OU). An OU is a container
-- of Amazon Web Services accounts within a root of an organization.
-- Policies that are attached to an OU apply to all accounts contained in
-- that OU and in any child OUs.
--
-- /See:/ 'newOrganizationalUnit' smart constructor.
data OrganizationalUnit = OrganizationalUnit'
  { -- | The friendly name of this OU.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of this OU.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /Amazon Web Services Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) associated with this OU.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an
    -- organizational unit ID string requires \"ou-\" followed by from 4 to 32
    -- lowercase letters or digits (the ID of the root that contains the OU).
    -- This string is followed by a second \"-\" dash and from 8 to 32
    -- additional lowercase letters or digits.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'organizationalUnit_name' - The friendly name of this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'arn', 'organizationalUnit_arn' - The Amazon Resource Name (ARN) of this OU.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
--
-- 'id', 'organizationalUnit_id' - The unique identifier (ID) associated with this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
newOrganizationalUnit ::
  OrganizationalUnit
newOrganizationalUnit =
  OrganizationalUnit'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The friendly name of this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
organizationalUnit_name :: Lens.Lens' OrganizationalUnit (Prelude.Maybe Prelude.Text)
organizationalUnit_name = Lens.lens (\OrganizationalUnit' {name} -> name) (\s@OrganizationalUnit' {} a -> s {name = a} :: OrganizationalUnit)

-- | The Amazon Resource Name (ARN) of this OU.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
organizationalUnit_arn :: Lens.Lens' OrganizationalUnit (Prelude.Maybe Prelude.Text)
organizationalUnit_arn = Lens.lens (\OrganizationalUnit' {arn} -> arn) (\s@OrganizationalUnit' {} a -> s {arn = a} :: OrganizationalUnit)

-- | The unique identifier (ID) associated with this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
organizationalUnit_id :: Lens.Lens' OrganizationalUnit (Prelude.Maybe Prelude.Text)
organizationalUnit_id = Lens.lens (\OrganizationalUnit' {id} -> id) (\s@OrganizationalUnit' {} a -> s {id = a} :: OrganizationalUnit)

instance Data.FromJSON OrganizationalUnit where
  parseJSON =
    Data.withObject
      "OrganizationalUnit"
      ( \x ->
          OrganizationalUnit'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable OrganizationalUnit where
  hashWithSalt _salt OrganizationalUnit' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData OrganizationalUnit where
  rnf OrganizationalUnit' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
