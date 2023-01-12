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
-- Module      : Amazonka.Lightsail.Types.AccessRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AccessRules where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AccessType
import qualified Amazonka.Prelude as Prelude

-- | Describes the anonymous access permissions for an Amazon Lightsail
-- bucket and its objects.
--
-- For more information about bucket access permissions, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-understanding-bucket-permissions Understanding bucket permissions in Amazon Lightsail>
-- in the
--
-- /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newAccessRules' smart constructor.
data AccessRules = AccessRules'
  { -- | A Boolean value that indicates whether the access control list (ACL)
    -- permissions that are applied to individual objects override the
    -- @getObject@ option that is currently specified.
    --
    -- When this is true, you can use the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectAcl.html PutObjectAcl>
    -- Amazon S3 API action to set individual objects to public (read-only)
    -- using the @public-read@ ACL, or to private using the @private@ ACL.
    allowPublicOverrides :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the anonymous access to all objects in a bucket.
    --
    -- The following options can be specified:
    --
    -- -   @public@ - Sets all objects in the bucket to public (read-only),
    --     making them readable by anyone in the world.
    --
    --     If the @getObject@ value is set to @public@, then all objects in the
    --     bucket default to public regardless of the @allowPublicOverrides@
    --     value.
    --
    -- -   @private@ - Sets all objects in the bucket to private, making them
    --     readable only by you or anyone you give access to.
    --
    --     If the @getObject@ value is set to @private@, and the
    --     @allowPublicOverrides@ value is set to @true@, then all objects in
    --     the bucket default to private unless they are configured with a
    --     @public-read@ ACL. Individual objects with a @public-read@ ACL are
    --     readable by anyone in the world.
    getObject :: Prelude.Maybe AccessType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowPublicOverrides', 'accessRules_allowPublicOverrides' - A Boolean value that indicates whether the access control list (ACL)
-- permissions that are applied to individual objects override the
-- @getObject@ option that is currently specified.
--
-- When this is true, you can use the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectAcl.html PutObjectAcl>
-- Amazon S3 API action to set individual objects to public (read-only)
-- using the @public-read@ ACL, or to private using the @private@ ACL.
--
-- 'getObject', 'accessRules_getObject' - Specifies the anonymous access to all objects in a bucket.
--
-- The following options can be specified:
--
-- -   @public@ - Sets all objects in the bucket to public (read-only),
--     making them readable by anyone in the world.
--
--     If the @getObject@ value is set to @public@, then all objects in the
--     bucket default to public regardless of the @allowPublicOverrides@
--     value.
--
-- -   @private@ - Sets all objects in the bucket to private, making them
--     readable only by you or anyone you give access to.
--
--     If the @getObject@ value is set to @private@, and the
--     @allowPublicOverrides@ value is set to @true@, then all objects in
--     the bucket default to private unless they are configured with a
--     @public-read@ ACL. Individual objects with a @public-read@ ACL are
--     readable by anyone in the world.
newAccessRules ::
  AccessRules
newAccessRules =
  AccessRules'
    { allowPublicOverrides =
        Prelude.Nothing,
      getObject = Prelude.Nothing
    }

-- | A Boolean value that indicates whether the access control list (ACL)
-- permissions that are applied to individual objects override the
-- @getObject@ option that is currently specified.
--
-- When this is true, you can use the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectAcl.html PutObjectAcl>
-- Amazon S3 API action to set individual objects to public (read-only)
-- using the @public-read@ ACL, or to private using the @private@ ACL.
accessRules_allowPublicOverrides :: Lens.Lens' AccessRules (Prelude.Maybe Prelude.Bool)
accessRules_allowPublicOverrides = Lens.lens (\AccessRules' {allowPublicOverrides} -> allowPublicOverrides) (\s@AccessRules' {} a -> s {allowPublicOverrides = a} :: AccessRules)

-- | Specifies the anonymous access to all objects in a bucket.
--
-- The following options can be specified:
--
-- -   @public@ - Sets all objects in the bucket to public (read-only),
--     making them readable by anyone in the world.
--
--     If the @getObject@ value is set to @public@, then all objects in the
--     bucket default to public regardless of the @allowPublicOverrides@
--     value.
--
-- -   @private@ - Sets all objects in the bucket to private, making them
--     readable only by you or anyone you give access to.
--
--     If the @getObject@ value is set to @private@, and the
--     @allowPublicOverrides@ value is set to @true@, then all objects in
--     the bucket default to private unless they are configured with a
--     @public-read@ ACL. Individual objects with a @public-read@ ACL are
--     readable by anyone in the world.
accessRules_getObject :: Lens.Lens' AccessRules (Prelude.Maybe AccessType)
accessRules_getObject = Lens.lens (\AccessRules' {getObject} -> getObject) (\s@AccessRules' {} a -> s {getObject = a} :: AccessRules)

instance Data.FromJSON AccessRules where
  parseJSON =
    Data.withObject
      "AccessRules"
      ( \x ->
          AccessRules'
            Prelude.<$> (x Data..:? "allowPublicOverrides")
            Prelude.<*> (x Data..:? "getObject")
      )

instance Prelude.Hashable AccessRules where
  hashWithSalt _salt AccessRules' {..} =
    _salt `Prelude.hashWithSalt` allowPublicOverrides
      `Prelude.hashWithSalt` getObject

instance Prelude.NFData AccessRules where
  rnf AccessRules' {..} =
    Prelude.rnf allowPublicOverrides
      `Prelude.seq` Prelude.rnf getObject

instance Data.ToJSON AccessRules where
  toJSON AccessRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowPublicOverrides" Data..=)
              Prelude.<$> allowPublicOverrides,
            ("getObject" Data..=) Prelude.<$> getObject
          ]
      )
