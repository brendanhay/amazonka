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
-- Module      : Amazonka.Location.GetMapSprites
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the sprite sheet corresponding to a map resource. The sprite
-- sheet is a PNG image paired with a JSON document describing the offsets
-- of individual icons that will be displayed on a rendered map.
module Amazonka.Location.GetMapSprites
  ( -- * Creating a Request
    GetMapSprites (..),
    newGetMapSprites,

    -- * Request Lenses
    getMapSprites_fileName,
    getMapSprites_mapName,

    -- * Destructuring the Response
    GetMapSpritesResponse (..),
    newGetMapSpritesResponse,

    -- * Response Lenses
    getMapSpritesResponse_blob,
    getMapSpritesResponse_contentType,
    getMapSpritesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMapSprites' smart constructor.
data GetMapSprites = GetMapSprites'
  { -- | The name of the sprite ﬁle. Use the following ﬁle names for the sprite
    -- sheet:
    --
    -- -   @sprites.png@
    --
    -- -   @sprites\@2x.png@ for high pixel density displays
    --
    -- For the JSON document containing image offsets. Use the following ﬁle
    -- names:
    --
    -- -   @sprites.json@
    --
    -- -   @sprites\@2x.json@ for high pixel density displays
    fileName :: Prelude.Text,
    -- | The map resource associated with the sprite ﬁle.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMapSprites' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileName', 'getMapSprites_fileName' - The name of the sprite ﬁle. Use the following ﬁle names for the sprite
-- sheet:
--
-- -   @sprites.png@
--
-- -   @sprites\@2x.png@ for high pixel density displays
--
-- For the JSON document containing image offsets. Use the following ﬁle
-- names:
--
-- -   @sprites.json@
--
-- -   @sprites\@2x.json@ for high pixel density displays
--
-- 'mapName', 'getMapSprites_mapName' - The map resource associated with the sprite ﬁle.
newGetMapSprites ::
  -- | 'fileName'
  Prelude.Text ->
  -- | 'mapName'
  Prelude.Text ->
  GetMapSprites
newGetMapSprites pFileName_ pMapName_ =
  GetMapSprites'
    { fileName = pFileName_,
      mapName = pMapName_
    }

-- | The name of the sprite ﬁle. Use the following ﬁle names for the sprite
-- sheet:
--
-- -   @sprites.png@
--
-- -   @sprites\@2x.png@ for high pixel density displays
--
-- For the JSON document containing image offsets. Use the following ﬁle
-- names:
--
-- -   @sprites.json@
--
-- -   @sprites\@2x.json@ for high pixel density displays
getMapSprites_fileName :: Lens.Lens' GetMapSprites Prelude.Text
getMapSprites_fileName = Lens.lens (\GetMapSprites' {fileName} -> fileName) (\s@GetMapSprites' {} a -> s {fileName = a} :: GetMapSprites)

-- | The map resource associated with the sprite ﬁle.
getMapSprites_mapName :: Lens.Lens' GetMapSprites Prelude.Text
getMapSprites_mapName = Lens.lens (\GetMapSprites' {mapName} -> mapName) (\s@GetMapSprites' {} a -> s {mapName = a} :: GetMapSprites)

instance Core.AWSRequest GetMapSprites where
  type
    AWSResponse GetMapSprites =
      GetMapSpritesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetMapSpritesResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMapSprites where
  hashWithSalt _salt GetMapSprites' {..} =
    _salt
      `Prelude.hashWithSalt` fileName
      `Prelude.hashWithSalt` mapName

instance Prelude.NFData GetMapSprites where
  rnf GetMapSprites' {..} =
    Prelude.rnf fileName `Prelude.seq`
      Prelude.rnf mapName

instance Data.ToHeaders GetMapSprites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMapSprites where
  toPath GetMapSprites' {..} =
    Prelude.mconcat
      [ "/maps/v0/maps/",
        Data.toBS mapName,
        "/sprites/",
        Data.toBS fileName
      ]

instance Data.ToQuery GetMapSprites where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMapSpritesResponse' smart constructor.
data GetMapSpritesResponse = GetMapSpritesResponse'
  { -- | Contains the body of the sprite sheet or JSON offset ﬁle.
    blob :: Prelude.Maybe Prelude.ByteString,
    -- | The content type of the sprite sheet and offsets. For example, the
    -- sprite sheet content type is @image\/png@, and the sprite offset JSON
    -- document is @application\/json@.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMapSpritesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blob', 'getMapSpritesResponse_blob' - Contains the body of the sprite sheet or JSON offset ﬁle.
--
-- 'contentType', 'getMapSpritesResponse_contentType' - The content type of the sprite sheet and offsets. For example, the
-- sprite sheet content type is @image\/png@, and the sprite offset JSON
-- document is @application\/json@.
--
-- 'httpStatus', 'getMapSpritesResponse_httpStatus' - The response's http status code.
newGetMapSpritesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMapSpritesResponse
newGetMapSpritesResponse pHttpStatus_ =
  GetMapSpritesResponse'
    { blob = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the body of the sprite sheet or JSON offset ﬁle.
getMapSpritesResponse_blob :: Lens.Lens' GetMapSpritesResponse (Prelude.Maybe Prelude.ByteString)
getMapSpritesResponse_blob = Lens.lens (\GetMapSpritesResponse' {blob} -> blob) (\s@GetMapSpritesResponse' {} a -> s {blob = a} :: GetMapSpritesResponse)

-- | The content type of the sprite sheet and offsets. For example, the
-- sprite sheet content type is @image\/png@, and the sprite offset JSON
-- document is @application\/json@.
getMapSpritesResponse_contentType :: Lens.Lens' GetMapSpritesResponse (Prelude.Maybe Prelude.Text)
getMapSpritesResponse_contentType = Lens.lens (\GetMapSpritesResponse' {contentType} -> contentType) (\s@GetMapSpritesResponse' {} a -> s {contentType = a} :: GetMapSpritesResponse)

-- | The response's http status code.
getMapSpritesResponse_httpStatus :: Lens.Lens' GetMapSpritesResponse Prelude.Int
getMapSpritesResponse_httpStatus = Lens.lens (\GetMapSpritesResponse' {httpStatus} -> httpStatus) (\s@GetMapSpritesResponse' {} a -> s {httpStatus = a} :: GetMapSpritesResponse)

instance Prelude.NFData GetMapSpritesResponse where
  rnf GetMapSpritesResponse' {..} =
    Prelude.rnf blob `Prelude.seq`
      Prelude.rnf contentType `Prelude.seq`
        Prelude.rnf httpStatus
