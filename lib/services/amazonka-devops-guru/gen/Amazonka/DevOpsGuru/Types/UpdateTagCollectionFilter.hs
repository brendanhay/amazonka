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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateTagCollectionFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateTagCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A new collection of Amazon Web Services resources that are defined by an
-- Amazon Web Services tag or tag /key/\//value/ pair.
--
-- /See:/ 'newUpdateTagCollectionFilter' smart constructor.
data UpdateTagCollectionFilter = UpdateTagCollectionFilter'
  { -- | An Amazon Web Services tag /key/ that is used to identify the Amazon Web
    -- Services resources that DevOps Guru analyzes. All Amazon Web Services
    -- resources in your account and Region tagged with this /key/ make up your
    -- DevOps Guru application and analysis boundary.
    --
    -- The string used for a /key/ in a tag that you use to define your
    -- resource coverage must begin with the prefix @Devops-guru-@. The tag
    -- /key/ might be @DevOps-Guru-deployment-application@ or
    -- @devops-guru-rds-application@. When you create a /key/, the case of
    -- characters in the /key/ can be whatever you choose. After you create a
    -- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
    -- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
    -- act as two different /keys/. Possible /key/\//value/ pairs in your
    -- application might be @Devops-Guru-production-application\/RDS@ or
    -- @Devops-Guru-production-application\/containers@.
    appBoundaryKey :: Prelude.Text,
    -- | The values in an Amazon Web Services tag collection.
    --
    -- The tag\'s /value/ is an optional field used to associate a string with
    -- the tag /key/ (for example, @111122223333@, @Production@, or a team
    -- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
    -- /value/ is the same as using an empty string. Like tag /keys/, tag
    -- /values/ are case-sensitive. You can specify a maximum of 256 characters
    -- for a tag value.
    tagValues :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTagCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBoundaryKey', 'updateTagCollectionFilter_appBoundaryKey' - An Amazon Web Services tag /key/ that is used to identify the Amazon Web
-- Services resources that DevOps Guru analyzes. All Amazon Web Services
-- resources in your account and Region tagged with this /key/ make up your
-- DevOps Guru application and analysis boundary.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
--
-- 'tagValues', 'updateTagCollectionFilter_tagValues' - The values in an Amazon Web Services tag collection.
--
-- The tag\'s /value/ is an optional field used to associate a string with
-- the tag /key/ (for example, @111122223333@, @Production@, or a team
-- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
-- /value/ is the same as using an empty string. Like tag /keys/, tag
-- /values/ are case-sensitive. You can specify a maximum of 256 characters
-- for a tag value.
newUpdateTagCollectionFilter ::
  -- | 'appBoundaryKey'
  Prelude.Text ->
  UpdateTagCollectionFilter
newUpdateTagCollectionFilter pAppBoundaryKey_ =
  UpdateTagCollectionFilter'
    { appBoundaryKey =
        pAppBoundaryKey_,
      tagValues = Prelude.mempty
    }

-- | An Amazon Web Services tag /key/ that is used to identify the Amazon Web
-- Services resources that DevOps Guru analyzes. All Amazon Web Services
-- resources in your account and Region tagged with this /key/ make up your
-- DevOps Guru application and analysis boundary.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
updateTagCollectionFilter_appBoundaryKey :: Lens.Lens' UpdateTagCollectionFilter Prelude.Text
updateTagCollectionFilter_appBoundaryKey = Lens.lens (\UpdateTagCollectionFilter' {appBoundaryKey} -> appBoundaryKey) (\s@UpdateTagCollectionFilter' {} a -> s {appBoundaryKey = a} :: UpdateTagCollectionFilter)

-- | The values in an Amazon Web Services tag collection.
--
-- The tag\'s /value/ is an optional field used to associate a string with
-- the tag /key/ (for example, @111122223333@, @Production@, or a team
-- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
-- /value/ is the same as using an empty string. Like tag /keys/, tag
-- /values/ are case-sensitive. You can specify a maximum of 256 characters
-- for a tag value.
updateTagCollectionFilter_tagValues :: Lens.Lens' UpdateTagCollectionFilter [Prelude.Text]
updateTagCollectionFilter_tagValues = Lens.lens (\UpdateTagCollectionFilter' {tagValues} -> tagValues) (\s@UpdateTagCollectionFilter' {} a -> s {tagValues = a} :: UpdateTagCollectionFilter) Prelude.. Lens.coerced

instance Prelude.Hashable UpdateTagCollectionFilter where
  hashWithSalt _salt UpdateTagCollectionFilter' {..} =
    _salt `Prelude.hashWithSalt` appBoundaryKey
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData UpdateTagCollectionFilter where
  rnf UpdateTagCollectionFilter' {..} =
    Prelude.rnf appBoundaryKey
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToJSON UpdateTagCollectionFilter where
  toJSON UpdateTagCollectionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppBoundaryKey" Data..= appBoundaryKey),
            Prelude.Just ("TagValues" Data..= tagValues)
          ]
      )
