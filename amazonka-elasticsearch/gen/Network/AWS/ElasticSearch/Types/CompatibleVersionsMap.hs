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
-- Module      : Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CompatibleVersionsMap where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A map from an @ ElasticsearchVersion @ to a list of compatible
-- @ ElasticsearchVersion @ s to which the domain can be upgraded.
--
-- /See:/ 'newCompatibleVersionsMap' smart constructor.
data CompatibleVersionsMap = CompatibleVersionsMap'
  { -- | The current version of Elasticsearch on which a domain is.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    targetVersions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CompatibleVersionsMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'compatibleVersionsMap_sourceVersion' - The current version of Elasticsearch on which a domain is.
--
-- 'targetVersions', 'compatibleVersionsMap_targetVersions' - Undocumented member.
newCompatibleVersionsMap ::
  CompatibleVersionsMap
newCompatibleVersionsMap =
  CompatibleVersionsMap'
    { sourceVersion =
        Prelude.Nothing,
      targetVersions = Prelude.Nothing
    }

-- | The current version of Elasticsearch on which a domain is.
compatibleVersionsMap_sourceVersion :: Lens.Lens' CompatibleVersionsMap (Prelude.Maybe Prelude.Text)
compatibleVersionsMap_sourceVersion = Lens.lens (\CompatibleVersionsMap' {sourceVersion} -> sourceVersion) (\s@CompatibleVersionsMap' {} a -> s {sourceVersion = a} :: CompatibleVersionsMap)

-- | Undocumented member.
compatibleVersionsMap_targetVersions :: Lens.Lens' CompatibleVersionsMap (Prelude.Maybe [Prelude.Text])
compatibleVersionsMap_targetVersions = Lens.lens (\CompatibleVersionsMap' {targetVersions} -> targetVersions) (\s@CompatibleVersionsMap' {} a -> s {targetVersions = a} :: CompatibleVersionsMap) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CompatibleVersionsMap where
  parseJSON =
    Prelude.withObject
      "CompatibleVersionsMap"
      ( \x ->
          CompatibleVersionsMap'
            Prelude.<$> (x Prelude..:? "SourceVersion")
            Prelude.<*> ( x Prelude..:? "TargetVersions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CompatibleVersionsMap

instance Prelude.NFData CompatibleVersionsMap
