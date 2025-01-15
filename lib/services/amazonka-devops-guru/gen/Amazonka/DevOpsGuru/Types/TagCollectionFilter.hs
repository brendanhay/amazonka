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
-- Module      : Amazonka.DevOpsGuru.Types.TagCollectionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.TagCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A collection of Amazon Web Services tags used to filter insights. This
-- is used to return insights generated from only resources that contain
-- the tags in the tag collection.
--
-- /See:/ 'newTagCollectionFilter' smart constructor.
data TagCollectionFilter = TagCollectionFilter'
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
-- Create a value of 'TagCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBoundaryKey', 'tagCollectionFilter_appBoundaryKey' - An Amazon Web Services tag /key/ that is used to identify the Amazon Web
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
-- 'tagValues', 'tagCollectionFilter_tagValues' - The values in an Amazon Web Services tag collection.
--
-- The tag\'s /value/ is an optional field used to associate a string with
-- the tag /key/ (for example, @111122223333@, @Production@, or a team
-- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
-- /value/ is the same as using an empty string. Like tag /keys/, tag
-- /values/ are case-sensitive. You can specify a maximum of 256 characters
-- for a tag value.
newTagCollectionFilter ::
  -- | 'appBoundaryKey'
  Prelude.Text ->
  TagCollectionFilter
newTagCollectionFilter pAppBoundaryKey_ =
  TagCollectionFilter'
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
tagCollectionFilter_appBoundaryKey :: Lens.Lens' TagCollectionFilter Prelude.Text
tagCollectionFilter_appBoundaryKey = Lens.lens (\TagCollectionFilter' {appBoundaryKey} -> appBoundaryKey) (\s@TagCollectionFilter' {} a -> s {appBoundaryKey = a} :: TagCollectionFilter)

-- | The values in an Amazon Web Services tag collection.
--
-- The tag\'s /value/ is an optional field used to associate a string with
-- the tag /key/ (for example, @111122223333@, @Production@, or a team
-- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
-- /value/ is the same as using an empty string. Like tag /keys/, tag
-- /values/ are case-sensitive. You can specify a maximum of 256 characters
-- for a tag value.
tagCollectionFilter_tagValues :: Lens.Lens' TagCollectionFilter [Prelude.Text]
tagCollectionFilter_tagValues = Lens.lens (\TagCollectionFilter' {tagValues} -> tagValues) (\s@TagCollectionFilter' {} a -> s {tagValues = a} :: TagCollectionFilter) Prelude.. Lens.coerced

instance Data.FromJSON TagCollectionFilter where
  parseJSON =
    Data.withObject
      "TagCollectionFilter"
      ( \x ->
          TagCollectionFilter'
            Prelude.<$> (x Data..: "AppBoundaryKey")
            Prelude.<*> (x Data..:? "TagValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TagCollectionFilter where
  hashWithSalt _salt TagCollectionFilter' {..} =
    _salt
      `Prelude.hashWithSalt` appBoundaryKey
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData TagCollectionFilter where
  rnf TagCollectionFilter' {..} =
    Prelude.rnf appBoundaryKey `Prelude.seq`
      Prelude.rnf tagValues
