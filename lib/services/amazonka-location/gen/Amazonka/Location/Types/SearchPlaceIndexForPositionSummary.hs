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
-- Module      : Amazonka.Location.Types.SearchPlaceIndexForPositionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchPlaceIndexForPositionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the request sent by using @SearchPlaceIndexForPosition@.
--
-- /See:/ 'newSearchPlaceIndexForPositionSummary' smart constructor.
data SearchPlaceIndexForPositionSummary = SearchPlaceIndexForPositionSummary'
  { -- | The preferred language used to return results. Matches the language in
    -- the request. The value is a valid
    -- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
    -- @en@ for English.
    language :: Prelude.Maybe Prelude.Text,
    -- | Contains the optional result count limit that is specified in the
    -- request.
    --
    -- Default value: @50@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The geospatial data provider attached to the place index resource
    -- specified in the request. Values can be one of the following:
    --
    -- -   Esri
    --
    -- -   Grab
    --
    -- -   Here
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The position specified in the request.
    position :: Data.Sensitive (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForPositionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'searchPlaceIndexForPositionSummary_language' - The preferred language used to return results. Matches the language in
-- the request. The value is a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- 'maxResults', 'searchPlaceIndexForPositionSummary_maxResults' - Contains the optional result count limit that is specified in the
-- request.
--
-- Default value: @50@
--
-- 'dataSource', 'searchPlaceIndexForPositionSummary_dataSource' - The geospatial data provider attached to the place index resource
-- specified in the request. Values can be one of the following:
--
-- -   Esri
--
-- -   Grab
--
-- -   Here
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'position', 'searchPlaceIndexForPositionSummary_position' - The position specified in the request.
newSearchPlaceIndexForPositionSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  SearchPlaceIndexForPositionSummary
newSearchPlaceIndexForPositionSummary
  pDataSource_
  pPosition_ =
    SearchPlaceIndexForPositionSummary'
      { language =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        dataSource = pDataSource_,
        position =
          Data._Sensitive
            Prelude.. Lens.coerced
            Lens.# pPosition_
      }

-- | The preferred language used to return results. Matches the language in
-- the request. The value is a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
searchPlaceIndexForPositionSummary_language :: Lens.Lens' SearchPlaceIndexForPositionSummary (Prelude.Maybe Prelude.Text)
searchPlaceIndexForPositionSummary_language = Lens.lens (\SearchPlaceIndexForPositionSummary' {language} -> language) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {language = a} :: SearchPlaceIndexForPositionSummary)

-- | Contains the optional result count limit that is specified in the
-- request.
--
-- Default value: @50@
searchPlaceIndexForPositionSummary_maxResults :: Lens.Lens' SearchPlaceIndexForPositionSummary (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForPositionSummary_maxResults = Lens.lens (\SearchPlaceIndexForPositionSummary' {maxResults} -> maxResults) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {maxResults = a} :: SearchPlaceIndexForPositionSummary)

-- | The geospatial data provider attached to the place index resource
-- specified in the request. Values can be one of the following:
--
-- -   Esri
--
-- -   Grab
--
-- -   Here
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
searchPlaceIndexForPositionSummary_dataSource :: Lens.Lens' SearchPlaceIndexForPositionSummary Prelude.Text
searchPlaceIndexForPositionSummary_dataSource = Lens.lens (\SearchPlaceIndexForPositionSummary' {dataSource} -> dataSource) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {dataSource = a} :: SearchPlaceIndexForPositionSummary)

-- | The position specified in the request.
searchPlaceIndexForPositionSummary_position :: Lens.Lens' SearchPlaceIndexForPositionSummary (Prelude.NonEmpty Prelude.Double)
searchPlaceIndexForPositionSummary_position = Lens.lens (\SearchPlaceIndexForPositionSummary' {position} -> position) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {position = a} :: SearchPlaceIndexForPositionSummary) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance
  Data.FromJSON
    SearchPlaceIndexForPositionSummary
  where
  parseJSON =
    Data.withObject
      "SearchPlaceIndexForPositionSummary"
      ( \x ->
          SearchPlaceIndexForPositionSummary'
            Prelude.<$> (x Data..:? "Language")
            Prelude.<*> (x Data..:? "MaxResults")
            Prelude.<*> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "Position")
      )

instance
  Prelude.Hashable
    SearchPlaceIndexForPositionSummary
  where
  hashWithSalt
    _salt
    SearchPlaceIndexForPositionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` language
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` position

instance
  Prelude.NFData
    SearchPlaceIndexForPositionSummary
  where
  rnf SearchPlaceIndexForPositionSummary' {..} =
    Prelude.rnf language
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf position
