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
-- Module      : Amazonka.MediaTailor.DescribeLiveSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The live source to describe.
module Amazonka.MediaTailor.DescribeLiveSource
  ( -- * Creating a Request
    DescribeLiveSource (..),
    newDescribeLiveSource,

    -- * Request Lenses
    describeLiveSource_liveSourceName,
    describeLiveSource_sourceLocationName,

    -- * Destructuring the Response
    DescribeLiveSourceResponse (..),
    newDescribeLiveSourceResponse,

    -- * Response Lenses
    describeLiveSourceResponse_arn,
    describeLiveSourceResponse_creationTime,
    describeLiveSourceResponse_httpPackageConfigurations,
    describeLiveSourceResponse_lastModifiedTime,
    describeLiveSourceResponse_liveSourceName,
    describeLiveSourceResponse_sourceLocationName,
    describeLiveSourceResponse_tags,
    describeLiveSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLiveSource' smart constructor.
data DescribeLiveSource = DescribeLiveSource'
  { -- | The name of the live source.
    liveSourceName :: Prelude.Text,
    -- | The name of the source location associated with this Live Source.
    sourceLocationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLiveSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'liveSourceName', 'describeLiveSource_liveSourceName' - The name of the live source.
--
-- 'sourceLocationName', 'describeLiveSource_sourceLocationName' - The name of the source location associated with this Live Source.
newDescribeLiveSource ::
  -- | 'liveSourceName'
  Prelude.Text ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  DescribeLiveSource
newDescribeLiveSource
  pLiveSourceName_
  pSourceLocationName_ =
    DescribeLiveSource'
      { liveSourceName =
          pLiveSourceName_,
        sourceLocationName = pSourceLocationName_
      }

-- | The name of the live source.
describeLiveSource_liveSourceName :: Lens.Lens' DescribeLiveSource Prelude.Text
describeLiveSource_liveSourceName = Lens.lens (\DescribeLiveSource' {liveSourceName} -> liveSourceName) (\s@DescribeLiveSource' {} a -> s {liveSourceName = a} :: DescribeLiveSource)

-- | The name of the source location associated with this Live Source.
describeLiveSource_sourceLocationName :: Lens.Lens' DescribeLiveSource Prelude.Text
describeLiveSource_sourceLocationName = Lens.lens (\DescribeLiveSource' {sourceLocationName} -> sourceLocationName) (\s@DescribeLiveSource' {} a -> s {sourceLocationName = a} :: DescribeLiveSource)

instance Core.AWSRequest DescribeLiveSource where
  type
    AWSResponse DescribeLiveSource =
      DescribeLiveSourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLiveSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> ( x Data..?> "HttpPackageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LiveSourceName")
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLiveSource where
  hashWithSalt _salt DescribeLiveSource' {..} =
    _salt `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData DescribeLiveSource where
  rnf DescribeLiveSource' {..} =
    Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders DescribeLiveSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeLiveSource where
  toPath DescribeLiveSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/liveSource/",
        Data.toBS liveSourceName
      ]

instance Data.ToQuery DescribeLiveSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLiveSourceResponse' smart constructor.
data DescribeLiveSourceResponse = DescribeLiveSourceResponse'
  { -- | The ARN of the live source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that indicates when the live source was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The HTTP package configurations.
    httpPackageConfigurations :: Prelude.Maybe [HttpPackageConfiguration],
    -- | The timestamp that indicates when the live source was modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the live source.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source location associated with the live source.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the live source. Tags are key-value pairs that you
    -- can associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLiveSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeLiveSourceResponse_arn' - The ARN of the live source.
--
-- 'creationTime', 'describeLiveSourceResponse_creationTime' - The timestamp that indicates when the live source was created.
--
-- 'httpPackageConfigurations', 'describeLiveSourceResponse_httpPackageConfigurations' - The HTTP package configurations.
--
-- 'lastModifiedTime', 'describeLiveSourceResponse_lastModifiedTime' - The timestamp that indicates when the live source was modified.
--
-- 'liveSourceName', 'describeLiveSourceResponse_liveSourceName' - The name of the live source.
--
-- 'sourceLocationName', 'describeLiveSourceResponse_sourceLocationName' - The name of the source location associated with the live source.
--
-- 'tags', 'describeLiveSourceResponse_tags' - The tags assigned to the live source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpStatus', 'describeLiveSourceResponse_httpStatus' - The response's http status code.
newDescribeLiveSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLiveSourceResponse
newDescribeLiveSourceResponse pHttpStatus_ =
  DescribeLiveSourceResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpPackageConfigurations = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      liveSourceName = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the live source.
describeLiveSourceResponse_arn :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe Prelude.Text)
describeLiveSourceResponse_arn = Lens.lens (\DescribeLiveSourceResponse' {arn} -> arn) (\s@DescribeLiveSourceResponse' {} a -> s {arn = a} :: DescribeLiveSourceResponse)

-- | The timestamp that indicates when the live source was created.
describeLiveSourceResponse_creationTime :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeLiveSourceResponse_creationTime = Lens.lens (\DescribeLiveSourceResponse' {creationTime} -> creationTime) (\s@DescribeLiveSourceResponse' {} a -> s {creationTime = a} :: DescribeLiveSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The HTTP package configurations.
describeLiveSourceResponse_httpPackageConfigurations :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe [HttpPackageConfiguration])
describeLiveSourceResponse_httpPackageConfigurations = Lens.lens (\DescribeLiveSourceResponse' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@DescribeLiveSourceResponse' {} a -> s {httpPackageConfigurations = a} :: DescribeLiveSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that indicates when the live source was modified.
describeLiveSourceResponse_lastModifiedTime :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeLiveSourceResponse_lastModifiedTime = Lens.lens (\DescribeLiveSourceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeLiveSourceResponse' {} a -> s {lastModifiedTime = a} :: DescribeLiveSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the live source.
describeLiveSourceResponse_liveSourceName :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe Prelude.Text)
describeLiveSourceResponse_liveSourceName = Lens.lens (\DescribeLiveSourceResponse' {liveSourceName} -> liveSourceName) (\s@DescribeLiveSourceResponse' {} a -> s {liveSourceName = a} :: DescribeLiveSourceResponse)

-- | The name of the source location associated with the live source.
describeLiveSourceResponse_sourceLocationName :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe Prelude.Text)
describeLiveSourceResponse_sourceLocationName = Lens.lens (\DescribeLiveSourceResponse' {sourceLocationName} -> sourceLocationName) (\s@DescribeLiveSourceResponse' {} a -> s {sourceLocationName = a} :: DescribeLiveSourceResponse)

-- | The tags assigned to the live source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
describeLiveSourceResponse_tags :: Lens.Lens' DescribeLiveSourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeLiveSourceResponse_tags = Lens.lens (\DescribeLiveSourceResponse' {tags} -> tags) (\s@DescribeLiveSourceResponse' {} a -> s {tags = a} :: DescribeLiveSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLiveSourceResponse_httpStatus :: Lens.Lens' DescribeLiveSourceResponse Prelude.Int
describeLiveSourceResponse_httpStatus = Lens.lens (\DescribeLiveSourceResponse' {httpStatus} -> httpStatus) (\s@DescribeLiveSourceResponse' {} a -> s {httpStatus = a} :: DescribeLiveSourceResponse)

instance Prelude.NFData DescribeLiveSourceResponse where
  rnf DescribeLiveSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
