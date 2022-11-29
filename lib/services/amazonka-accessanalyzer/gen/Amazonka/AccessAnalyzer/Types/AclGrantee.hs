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
-- Module      : Amazonka.AccessAnalyzer.Types.AclGrantee
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AclGrantee where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | You specify each grantee as a type-value pair using one of these types.
-- You can specify only one type of grantee. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAcl.html PutBucketAcl>.
--
-- /See:/ 'newAclGrantee' smart constructor.
data AclGrantee = AclGrantee'
  { -- | Used for granting permissions to a predefined group.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The value specified is the canonical user ID of an Amazon Web Services
    -- account.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AclGrantee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'aclGrantee_uri' - Used for granting permissions to a predefined group.
--
-- 'id', 'aclGrantee_id' - The value specified is the canonical user ID of an Amazon Web Services
-- account.
newAclGrantee ::
  AclGrantee
newAclGrantee =
  AclGrantee'
    { uri = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Used for granting permissions to a predefined group.
aclGrantee_uri :: Lens.Lens' AclGrantee (Prelude.Maybe Prelude.Text)
aclGrantee_uri = Lens.lens (\AclGrantee' {uri} -> uri) (\s@AclGrantee' {} a -> s {uri = a} :: AclGrantee)

-- | The value specified is the canonical user ID of an Amazon Web Services
-- account.
aclGrantee_id :: Lens.Lens' AclGrantee (Prelude.Maybe Prelude.Text)
aclGrantee_id = Lens.lens (\AclGrantee' {id} -> id) (\s@AclGrantee' {} a -> s {id = a} :: AclGrantee)

instance Core.FromJSON AclGrantee where
  parseJSON =
    Core.withObject
      "AclGrantee"
      ( \x ->
          AclGrantee'
            Prelude.<$> (x Core..:? "uri") Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable AclGrantee where
  hashWithSalt _salt AclGrantee' {..} =
    _salt `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` id

instance Prelude.NFData AclGrantee where
  rnf AclGrantee' {..} =
    Prelude.rnf uri `Prelude.seq` Prelude.rnf id

instance Core.ToJSON AclGrantee where
  toJSON AclGrantee' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("uri" Core..=) Prelude.<$> uri,
            ("id" Core..=) Prelude.<$> id
          ]
      )
