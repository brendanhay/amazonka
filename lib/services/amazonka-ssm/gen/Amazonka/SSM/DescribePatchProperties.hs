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
-- Module      : Amazonka.SSM.DescribePatchProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of available patches organized by product, product
-- family, classification, severity, and other properties of available
-- patches. You can use the reported properties in the filters you specify
-- in requests for operations such as CreatePatchBaseline,
-- UpdatePatchBaseline, DescribeAvailablePatches, and
-- DescribePatchBaselines.
--
-- The following section lists the properties that can be used in filters
-- for each major operating system type:
--
-- [AMAZON_LINUX]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@ | @SEVERITY@
--
-- [AMAZON_LINUX_2]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@ | @SEVERITY@
--
-- [CENTOS]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@ | @SEVERITY@
--
-- [DEBIAN]
--     Valid properties: @PRODUCT@ | @PRIORITY@
--
-- [MACOS]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@
--
-- [ORACLE_LINUX]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@ | @SEVERITY@
--
-- [REDHAT_ENTERPRISE_LINUX]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@ | @SEVERITY@
--
-- [SUSE]
--     Valid properties: @PRODUCT@ | @CLASSIFICATION@ | @SEVERITY@
--
-- [UBUNTU]
--     Valid properties: @PRODUCT@ | @PRIORITY@
--
-- [WINDOWS]
--     Valid properties: @PRODUCT@ | @PRODUCT_FAMILY@ | @CLASSIFICATION@ |
--     @MSRC_SEVERITY@
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribePatchProperties
  ( -- * Creating a Request
    DescribePatchProperties (..),
    newDescribePatchProperties,

    -- * Request Lenses
    describePatchProperties_maxResults,
    describePatchProperties_nextToken,
    describePatchProperties_patchSet,
    describePatchProperties_operatingSystem,
    describePatchProperties_property,

    -- * Destructuring the Response
    DescribePatchPropertiesResponse (..),
    newDescribePatchPropertiesResponse,

    -- * Response Lenses
    describePatchPropertiesResponse_nextToken,
    describePatchPropertiesResponse_properties,
    describePatchPropertiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribePatchProperties' smart constructor.
data DescribePatchProperties = DescribePatchProperties'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to list patches for the Windows operating system or
    -- for applications released by Microsoft. Not applicable for the Linux or
    -- macOS operating systems.
    patchSet :: Prelude.Maybe PatchSet,
    -- | The operating system type for which to list patches.
    operatingSystem :: OperatingSystem,
    -- | The patch property for which you want to view patch details.
    property :: PatchProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describePatchProperties_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describePatchProperties_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'patchSet', 'describePatchProperties_patchSet' - Indicates whether to list patches for the Windows operating system or
-- for applications released by Microsoft. Not applicable for the Linux or
-- macOS operating systems.
--
-- 'operatingSystem', 'describePatchProperties_operatingSystem' - The operating system type for which to list patches.
--
-- 'property', 'describePatchProperties_property' - The patch property for which you want to view patch details.
newDescribePatchProperties ::
  -- | 'operatingSystem'
  OperatingSystem ->
  -- | 'property'
  PatchProperty ->
  DescribePatchProperties
newDescribePatchProperties
  pOperatingSystem_
  pProperty_ =
    DescribePatchProperties'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        patchSet = Prelude.Nothing,
        operatingSystem = pOperatingSystem_,
        property = pProperty_
      }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describePatchProperties_maxResults :: Lens.Lens' DescribePatchProperties (Prelude.Maybe Prelude.Natural)
describePatchProperties_maxResults = Lens.lens (\DescribePatchProperties' {maxResults} -> maxResults) (\s@DescribePatchProperties' {} a -> s {maxResults = a} :: DescribePatchProperties)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePatchProperties_nextToken :: Lens.Lens' DescribePatchProperties (Prelude.Maybe Prelude.Text)
describePatchProperties_nextToken = Lens.lens (\DescribePatchProperties' {nextToken} -> nextToken) (\s@DescribePatchProperties' {} a -> s {nextToken = a} :: DescribePatchProperties)

-- | Indicates whether to list patches for the Windows operating system or
-- for applications released by Microsoft. Not applicable for the Linux or
-- macOS operating systems.
describePatchProperties_patchSet :: Lens.Lens' DescribePatchProperties (Prelude.Maybe PatchSet)
describePatchProperties_patchSet = Lens.lens (\DescribePatchProperties' {patchSet} -> patchSet) (\s@DescribePatchProperties' {} a -> s {patchSet = a} :: DescribePatchProperties)

-- | The operating system type for which to list patches.
describePatchProperties_operatingSystem :: Lens.Lens' DescribePatchProperties OperatingSystem
describePatchProperties_operatingSystem = Lens.lens (\DescribePatchProperties' {operatingSystem} -> operatingSystem) (\s@DescribePatchProperties' {} a -> s {operatingSystem = a} :: DescribePatchProperties)

-- | The patch property for which you want to view patch details.
describePatchProperties_property :: Lens.Lens' DescribePatchProperties PatchProperty
describePatchProperties_property = Lens.lens (\DescribePatchProperties' {property} -> property) (\s@DescribePatchProperties' {} a -> s {property = a} :: DescribePatchProperties)

instance Core.AWSPager DescribePatchProperties where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePatchPropertiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePatchPropertiesResponse_properties
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describePatchProperties_nextToken
          Lens..~ rs
          Lens.^? describePatchPropertiesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribePatchProperties where
  type
    AWSResponse DescribePatchProperties =
      DescribePatchPropertiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchPropertiesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePatchProperties where
  hashWithSalt _salt DescribePatchProperties' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` patchSet
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` property

instance Prelude.NFData DescribePatchProperties where
  rnf DescribePatchProperties' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf patchSet
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf property

instance Data.ToHeaders DescribePatchProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribePatchProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePatchProperties where
  toJSON DescribePatchProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PatchSet" Data..=) Prelude.<$> patchSet,
            Prelude.Just
              ("OperatingSystem" Data..= operatingSystem),
            Prelude.Just ("Property" Data..= property)
          ]
      )

instance Data.ToPath DescribePatchProperties where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePatchProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePatchPropertiesResponse' smart constructor.
data DescribePatchPropertiesResponse = DescribePatchPropertiesResponse'
  { -- | The token for the next set of items to return. (You use this token in
    -- the next call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the properties for patches matching the filter request
    -- parameters.
    properties :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchPropertiesResponse_nextToken' - The token for the next set of items to return. (You use this token in
-- the next call.)
--
-- 'properties', 'describePatchPropertiesResponse_properties' - A list of the properties for patches matching the filter request
-- parameters.
--
-- 'httpStatus', 'describePatchPropertiesResponse_httpStatus' - The response's http status code.
newDescribePatchPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePatchPropertiesResponse
newDescribePatchPropertiesResponse pHttpStatus_ =
  DescribePatchPropertiesResponse'
    { nextToken =
        Prelude.Nothing,
      properties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. (You use this token in
-- the next call.)
describePatchPropertiesResponse_nextToken :: Lens.Lens' DescribePatchPropertiesResponse (Prelude.Maybe Prelude.Text)
describePatchPropertiesResponse_nextToken = Lens.lens (\DescribePatchPropertiesResponse' {nextToken} -> nextToken) (\s@DescribePatchPropertiesResponse' {} a -> s {nextToken = a} :: DescribePatchPropertiesResponse)

-- | A list of the properties for patches matching the filter request
-- parameters.
describePatchPropertiesResponse_properties :: Lens.Lens' DescribePatchPropertiesResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
describePatchPropertiesResponse_properties = Lens.lens (\DescribePatchPropertiesResponse' {properties} -> properties) (\s@DescribePatchPropertiesResponse' {} a -> s {properties = a} :: DescribePatchPropertiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePatchPropertiesResponse_httpStatus :: Lens.Lens' DescribePatchPropertiesResponse Prelude.Int
describePatchPropertiesResponse_httpStatus = Lens.lens (\DescribePatchPropertiesResponse' {httpStatus} -> httpStatus) (\s@DescribePatchPropertiesResponse' {} a -> s {httpStatus = a} :: DescribePatchPropertiesResponse)

instance
  Prelude.NFData
    DescribePatchPropertiesResponse
  where
  rnf DescribePatchPropertiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf httpStatus
