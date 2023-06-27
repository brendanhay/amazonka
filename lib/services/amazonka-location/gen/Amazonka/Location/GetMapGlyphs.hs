{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.GetMapGlyphs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves glyphs used to display labels on a map.
module Amazonka.Location.GetMapGlyphs
  ( -- * Creating a Request
    GetMapGlyphs (..),
    newGetMapGlyphs,

    -- * Request Lenses
    getMapGlyphs_key,
    getMapGlyphs_fontStack,
    getMapGlyphs_fontUnicodeRange,
    getMapGlyphs_mapName,

    -- * Destructuring the Response
    GetMapGlyphsResponse (..),
    newGetMapGlyphsResponse,

    -- * Response Lenses
    getMapGlyphsResponse_blob,
    getMapGlyphsResponse_cacheControl,
    getMapGlyphsResponse_contentType,
    getMapGlyphsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMapGlyphs' smart constructor.
data GetMapGlyphs = GetMapGlyphs'
  { -- | The optional
    -- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html API key>
    -- to authorize the request.
    key :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A comma-separated list of fonts to load glyphs from in order of
    -- preference. For example, @Noto Sans Regular, Arial Unicode@.
    --
    -- Valid fonts stacks for
    -- <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>
    -- styles:
    --
    -- -   VectorEsriDarkGrayCanvas – @Ubuntu Medium Italic@ | @Ubuntu Medium@
    --     | @Ubuntu Italic@ | @Ubuntu Regular@ | @Ubuntu Bold@
    --
    -- -   VectorEsriLightGrayCanvas – @Ubuntu Italic@ | @Ubuntu Regular@ |
    --     @Ubuntu Light@ | @Ubuntu Bold@
    --
    -- -   VectorEsriTopographic – @Noto Sans Italic@ | @Noto Sans Regular@ |
    --     @Noto Sans Bold@ | @Noto Serif Regular@ |
    --     @Roboto Condensed Light Italic@
    --
    -- -   VectorEsriStreets – @Arial Regular@ | @Arial Italic@ | @Arial Bold@
    --
    -- -   VectorEsriNavigation – @Arial Regular@ | @Arial Italic@ |
    --     @Arial Bold@
    --
    -- Valid font stacks for
    -- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>
    -- styles:
    --
    -- -   VectorHereContrast – @Fira GO Regular@ | @Fira GO Bold@
    --
    -- -   VectorHereExplore, VectorHereExploreTruck,
    --     HybridHereExploreSatellite – @Fira GO Italic@ | @Fira GO Map@ |
    --     @Fira GO Map Bold@ | @Noto Sans CJK JP Bold@ |
    --     @Noto Sans CJK JP Light@ | @Noto Sans CJK JP Regular@
    --
    -- Valid font stacks for
    -- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps>
    -- styles:
    --
    -- -   VectorGrabStandardLight, VectorGrabStandardDark –
    --     @Noto Sans Regular@ | @Noto Sans Medium@ | @Noto Sans Bold@
    --
    -- Valid font stacks for
    -- <https://docs.aws.amazon.com/location/latest/developerguide/open-data.html Open Data>
    -- styles:
    --
    -- -   VectorOpenDataStandardLight, VectorOpenDataStandardDark,
    --     VectorOpenDataVisualizationLight, VectorOpenDataVisualizationDark –
    --     @Amazon Ember Regular,Noto Sans Regular@ |
    --     @Amazon Ember Bold,Noto Sans Bold@ |
    --     @Amazon Ember Medium,Noto Sans Medium@ |
    --     @Amazon Ember Regular Italic,Noto Sans Italic@ |
    --     @Amazon Ember Condensed RC Regular,Noto Sans Regular@ |
    --     @Amazon Ember Condensed RC Bold,Noto Sans Bold@ |
    --     @Amazon Ember Regular,Noto Sans Regular,Noto Sans Arabic Regular@ |
    --     @Amazon Ember Condensed RC Bold,Noto Sans Bold,Noto Sans Arabic Condensed Bold@
    --     | @Amazon Ember Bold,Noto Sans Bold,Noto Sans Arabic Bold@ |
    --     @Amazon Ember Regular Italic,Noto Sans Italic,Noto Sans Arabic Regular@
    --     |
    --     @Amazon Ember Condensed RC Regular,Noto Sans Regular,Noto Sans Arabic Condensed Regular@
    --     | @Amazon Ember Medium,Noto Sans Medium,Noto Sans Arabic Medium@
    --
    -- The fonts used by the Open Data map styles are combined fonts that use
    -- @Amazon Ember@ for most glyphs but @Noto Sans@ for glyphs unsupported by
    -- @Amazon Ember@.
    fontStack :: Prelude.Text,
    -- | A Unicode range of characters to download glyphs for. Each response will
    -- contain 256 characters. For example, 0–255 includes all characters from
    -- range @U+0000@ to @00FF@. Must be aligned to multiples of 256.
    fontUnicodeRange :: Prelude.Text,
    -- | The map resource associated with the glyph ﬁle.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMapGlyphs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'getMapGlyphs_key' - The optional
-- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html API key>
-- to authorize the request.
--
-- 'fontStack', 'getMapGlyphs_fontStack' - A comma-separated list of fonts to load glyphs from in order of
-- preference. For example, @Noto Sans Regular, Arial Unicode@.
--
-- Valid fonts stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>
-- styles:
--
-- -   VectorEsriDarkGrayCanvas – @Ubuntu Medium Italic@ | @Ubuntu Medium@
--     | @Ubuntu Italic@ | @Ubuntu Regular@ | @Ubuntu Bold@
--
-- -   VectorEsriLightGrayCanvas – @Ubuntu Italic@ | @Ubuntu Regular@ |
--     @Ubuntu Light@ | @Ubuntu Bold@
--
-- -   VectorEsriTopographic – @Noto Sans Italic@ | @Noto Sans Regular@ |
--     @Noto Sans Bold@ | @Noto Serif Regular@ |
--     @Roboto Condensed Light Italic@
--
-- -   VectorEsriStreets – @Arial Regular@ | @Arial Italic@ | @Arial Bold@
--
-- -   VectorEsriNavigation – @Arial Regular@ | @Arial Italic@ |
--     @Arial Bold@
--
-- Valid font stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>
-- styles:
--
-- -   VectorHereContrast – @Fira GO Regular@ | @Fira GO Bold@
--
-- -   VectorHereExplore, VectorHereExploreTruck,
--     HybridHereExploreSatellite – @Fira GO Italic@ | @Fira GO Map@ |
--     @Fira GO Map Bold@ | @Noto Sans CJK JP Bold@ |
--     @Noto Sans CJK JP Light@ | @Noto Sans CJK JP Regular@
--
-- Valid font stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps>
-- styles:
--
-- -   VectorGrabStandardLight, VectorGrabStandardDark –
--     @Noto Sans Regular@ | @Noto Sans Medium@ | @Noto Sans Bold@
--
-- Valid font stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/open-data.html Open Data>
-- styles:
--
-- -   VectorOpenDataStandardLight, VectorOpenDataStandardDark,
--     VectorOpenDataVisualizationLight, VectorOpenDataVisualizationDark –
--     @Amazon Ember Regular,Noto Sans Regular@ |
--     @Amazon Ember Bold,Noto Sans Bold@ |
--     @Amazon Ember Medium,Noto Sans Medium@ |
--     @Amazon Ember Regular Italic,Noto Sans Italic@ |
--     @Amazon Ember Condensed RC Regular,Noto Sans Regular@ |
--     @Amazon Ember Condensed RC Bold,Noto Sans Bold@ |
--     @Amazon Ember Regular,Noto Sans Regular,Noto Sans Arabic Regular@ |
--     @Amazon Ember Condensed RC Bold,Noto Sans Bold,Noto Sans Arabic Condensed Bold@
--     | @Amazon Ember Bold,Noto Sans Bold,Noto Sans Arabic Bold@ |
--     @Amazon Ember Regular Italic,Noto Sans Italic,Noto Sans Arabic Regular@
--     |
--     @Amazon Ember Condensed RC Regular,Noto Sans Regular,Noto Sans Arabic Condensed Regular@
--     | @Amazon Ember Medium,Noto Sans Medium,Noto Sans Arabic Medium@
--
-- The fonts used by the Open Data map styles are combined fonts that use
-- @Amazon Ember@ for most glyphs but @Noto Sans@ for glyphs unsupported by
-- @Amazon Ember@.
--
-- 'fontUnicodeRange', 'getMapGlyphs_fontUnicodeRange' - A Unicode range of characters to download glyphs for. Each response will
-- contain 256 characters. For example, 0–255 includes all characters from
-- range @U+0000@ to @00FF@. Must be aligned to multiples of 256.
--
-- 'mapName', 'getMapGlyphs_mapName' - The map resource associated with the glyph ﬁle.
newGetMapGlyphs ::
  -- | 'fontStack'
  Prelude.Text ->
  -- | 'fontUnicodeRange'
  Prelude.Text ->
  -- | 'mapName'
  Prelude.Text ->
  GetMapGlyphs
newGetMapGlyphs
  pFontStack_
  pFontUnicodeRange_
  pMapName_ =
    GetMapGlyphs'
      { key = Prelude.Nothing,
        fontStack = pFontStack_,
        fontUnicodeRange = pFontUnicodeRange_,
        mapName = pMapName_
      }

-- | The optional
-- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html API key>
-- to authorize the request.
getMapGlyphs_key :: Lens.Lens' GetMapGlyphs (Prelude.Maybe Prelude.Text)
getMapGlyphs_key = Lens.lens (\GetMapGlyphs' {key} -> key) (\s@GetMapGlyphs' {} a -> s {key = a} :: GetMapGlyphs) Prelude.. Lens.mapping Data._Sensitive

-- | A comma-separated list of fonts to load glyphs from in order of
-- preference. For example, @Noto Sans Regular, Arial Unicode@.
--
-- Valid fonts stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>
-- styles:
--
-- -   VectorEsriDarkGrayCanvas – @Ubuntu Medium Italic@ | @Ubuntu Medium@
--     | @Ubuntu Italic@ | @Ubuntu Regular@ | @Ubuntu Bold@
--
-- -   VectorEsriLightGrayCanvas – @Ubuntu Italic@ | @Ubuntu Regular@ |
--     @Ubuntu Light@ | @Ubuntu Bold@
--
-- -   VectorEsriTopographic – @Noto Sans Italic@ | @Noto Sans Regular@ |
--     @Noto Sans Bold@ | @Noto Serif Regular@ |
--     @Roboto Condensed Light Italic@
--
-- -   VectorEsriStreets – @Arial Regular@ | @Arial Italic@ | @Arial Bold@
--
-- -   VectorEsriNavigation – @Arial Regular@ | @Arial Italic@ |
--     @Arial Bold@
--
-- Valid font stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>
-- styles:
--
-- -   VectorHereContrast – @Fira GO Regular@ | @Fira GO Bold@
--
-- -   VectorHereExplore, VectorHereExploreTruck,
--     HybridHereExploreSatellite – @Fira GO Italic@ | @Fira GO Map@ |
--     @Fira GO Map Bold@ | @Noto Sans CJK JP Bold@ |
--     @Noto Sans CJK JP Light@ | @Noto Sans CJK JP Regular@
--
-- Valid font stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps>
-- styles:
--
-- -   VectorGrabStandardLight, VectorGrabStandardDark –
--     @Noto Sans Regular@ | @Noto Sans Medium@ | @Noto Sans Bold@
--
-- Valid font stacks for
-- <https://docs.aws.amazon.com/location/latest/developerguide/open-data.html Open Data>
-- styles:
--
-- -   VectorOpenDataStandardLight, VectorOpenDataStandardDark,
--     VectorOpenDataVisualizationLight, VectorOpenDataVisualizationDark –
--     @Amazon Ember Regular,Noto Sans Regular@ |
--     @Amazon Ember Bold,Noto Sans Bold@ |
--     @Amazon Ember Medium,Noto Sans Medium@ |
--     @Amazon Ember Regular Italic,Noto Sans Italic@ |
--     @Amazon Ember Condensed RC Regular,Noto Sans Regular@ |
--     @Amazon Ember Condensed RC Bold,Noto Sans Bold@ |
--     @Amazon Ember Regular,Noto Sans Regular,Noto Sans Arabic Regular@ |
--     @Amazon Ember Condensed RC Bold,Noto Sans Bold,Noto Sans Arabic Condensed Bold@
--     | @Amazon Ember Bold,Noto Sans Bold,Noto Sans Arabic Bold@ |
--     @Amazon Ember Regular Italic,Noto Sans Italic,Noto Sans Arabic Regular@
--     |
--     @Amazon Ember Condensed RC Regular,Noto Sans Regular,Noto Sans Arabic Condensed Regular@
--     | @Amazon Ember Medium,Noto Sans Medium,Noto Sans Arabic Medium@
--
-- The fonts used by the Open Data map styles are combined fonts that use
-- @Amazon Ember@ for most glyphs but @Noto Sans@ for glyphs unsupported by
-- @Amazon Ember@.
getMapGlyphs_fontStack :: Lens.Lens' GetMapGlyphs Prelude.Text
getMapGlyphs_fontStack = Lens.lens (\GetMapGlyphs' {fontStack} -> fontStack) (\s@GetMapGlyphs' {} a -> s {fontStack = a} :: GetMapGlyphs)

-- | A Unicode range of characters to download glyphs for. Each response will
-- contain 256 characters. For example, 0–255 includes all characters from
-- range @U+0000@ to @00FF@. Must be aligned to multiples of 256.
getMapGlyphs_fontUnicodeRange :: Lens.Lens' GetMapGlyphs Prelude.Text
getMapGlyphs_fontUnicodeRange = Lens.lens (\GetMapGlyphs' {fontUnicodeRange} -> fontUnicodeRange) (\s@GetMapGlyphs' {} a -> s {fontUnicodeRange = a} :: GetMapGlyphs)

-- | The map resource associated with the glyph ﬁle.
getMapGlyphs_mapName :: Lens.Lens' GetMapGlyphs Prelude.Text
getMapGlyphs_mapName = Lens.lens (\GetMapGlyphs' {mapName} -> mapName) (\s@GetMapGlyphs' {} a -> s {mapName = a} :: GetMapGlyphs)

instance Core.AWSRequest GetMapGlyphs where
  type AWSResponse GetMapGlyphs = GetMapGlyphsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetMapGlyphsResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Data..#? "Cache-Control")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMapGlyphs where
  hashWithSalt _salt GetMapGlyphs' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` fontStack
      `Prelude.hashWithSalt` fontUnicodeRange
      `Prelude.hashWithSalt` mapName

instance Prelude.NFData GetMapGlyphs where
  rnf GetMapGlyphs' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf fontStack
      `Prelude.seq` Prelude.rnf fontUnicodeRange
      `Prelude.seq` Prelude.rnf mapName

instance Data.ToHeaders GetMapGlyphs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMapGlyphs where
  toPath GetMapGlyphs' {..} =
    Prelude.mconcat
      [ "/maps/v0/maps/",
        Data.toBS mapName,
        "/glyphs/",
        Data.toBS fontStack,
        "/",
        Data.toBS fontUnicodeRange
      ]

instance Data.ToQuery GetMapGlyphs where
  toQuery GetMapGlyphs' {..} =
    Prelude.mconcat ["key" Data.=: key]

-- | /See:/ 'newGetMapGlyphsResponse' smart constructor.
data GetMapGlyphsResponse = GetMapGlyphsResponse'
  { -- | The glyph, as binary blob.
    blob :: Prelude.Maybe Prelude.ByteString,
    -- | The HTTP Cache-Control directive for the value.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The map glyph content type. For example, @application\/octet-stream@.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMapGlyphsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blob', 'getMapGlyphsResponse_blob' - The glyph, as binary blob.
--
-- 'cacheControl', 'getMapGlyphsResponse_cacheControl' - The HTTP Cache-Control directive for the value.
--
-- 'contentType', 'getMapGlyphsResponse_contentType' - The map glyph content type. For example, @application\/octet-stream@.
--
-- 'httpStatus', 'getMapGlyphsResponse_httpStatus' - The response's http status code.
newGetMapGlyphsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMapGlyphsResponse
newGetMapGlyphsResponse pHttpStatus_ =
  GetMapGlyphsResponse'
    { blob = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The glyph, as binary blob.
getMapGlyphsResponse_blob :: Lens.Lens' GetMapGlyphsResponse (Prelude.Maybe Prelude.ByteString)
getMapGlyphsResponse_blob = Lens.lens (\GetMapGlyphsResponse' {blob} -> blob) (\s@GetMapGlyphsResponse' {} a -> s {blob = a} :: GetMapGlyphsResponse)

-- | The HTTP Cache-Control directive for the value.
getMapGlyphsResponse_cacheControl :: Lens.Lens' GetMapGlyphsResponse (Prelude.Maybe Prelude.Text)
getMapGlyphsResponse_cacheControl = Lens.lens (\GetMapGlyphsResponse' {cacheControl} -> cacheControl) (\s@GetMapGlyphsResponse' {} a -> s {cacheControl = a} :: GetMapGlyphsResponse)

-- | The map glyph content type. For example, @application\/octet-stream@.
getMapGlyphsResponse_contentType :: Lens.Lens' GetMapGlyphsResponse (Prelude.Maybe Prelude.Text)
getMapGlyphsResponse_contentType = Lens.lens (\GetMapGlyphsResponse' {contentType} -> contentType) (\s@GetMapGlyphsResponse' {} a -> s {contentType = a} :: GetMapGlyphsResponse)

-- | The response's http status code.
getMapGlyphsResponse_httpStatus :: Lens.Lens' GetMapGlyphsResponse Prelude.Int
getMapGlyphsResponse_httpStatus = Lens.lens (\GetMapGlyphsResponse' {httpStatus} -> httpStatus) (\s@GetMapGlyphsResponse' {} a -> s {httpStatus = a} :: GetMapGlyphsResponse)

instance Prelude.NFData GetMapGlyphsResponse where
  rnf GetMapGlyphsResponse' {..} =
    Prelude.rnf blob
      `Prelude.seq` Prelude.rnf cacheControl
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf httpStatus
