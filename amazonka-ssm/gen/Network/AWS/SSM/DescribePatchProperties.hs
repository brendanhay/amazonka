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
-- Module      : Network.AWS.SSM.DescribePatchProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of available patches organized by product, product
-- family, classification, severity, and other properties of available
-- patches. You can use the reported properties in the filters you specify
-- in requests for actions such as CreatePatchBaseline,
-- UpdatePatchBaseline, DescribeAvailablePatches, and
-- DescribePatchBaselines.
--
-- The following section lists the properties that can be used in filters
-- for each major operating system type:
--
-- [AMAZON_LINUX]
--     Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
-- [AMAZON_LINUX_2]
--     Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
-- [CENTOS]
--     Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
-- [DEBIAN]
--     Valid properties: PRODUCT, PRIORITY
--
-- [MACOS]
--     Valid properties: PRODUCT, CLASSIFICATION
--
-- [ORACLE_LINUX]
--     Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
-- [REDHAT_ENTERPRISE_LINUX]
--     Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
-- [SUSE]
--     Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
-- [UBUNTU]
--     Valid properties: PRODUCT, PRIORITY
--
-- [WINDOWS]
--     Valid properties: PRODUCT, PRODUCT_FAMILY, CLASSIFICATION,
--     MSRC_SEVERITY
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchProperties
  ( -- * Creating a Request
    DescribePatchProperties (..),
    newDescribePatchProperties,

    -- * Request Lenses
    describePatchProperties_nextToken,
    describePatchProperties_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribePatchProperties' smart constructor.
data DescribePatchProperties = DescribePatchProperties'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Indicates whether to list patches for the Windows operating system or
    -- for Microsoft applications. Not applicable for the Linux or macOS
    -- operating systems.
    patchSet :: Core.Maybe PatchSet,
    -- | The operating system type for which to list patches.
    operatingSystem :: OperatingSystem,
    -- | The patch property for which you want to view patch details.
    property :: PatchProperty
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePatchProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchProperties_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describePatchProperties_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'patchSet', 'describePatchProperties_patchSet' - Indicates whether to list patches for the Windows operating system or
-- for Microsoft applications. Not applicable for the Linux or macOS
-- operating systems.
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
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        patchSet = Core.Nothing,
        operatingSystem = pOperatingSystem_,
        property = pProperty_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePatchProperties_nextToken :: Lens.Lens' DescribePatchProperties (Core.Maybe Core.Text)
describePatchProperties_nextToken = Lens.lens (\DescribePatchProperties' {nextToken} -> nextToken) (\s@DescribePatchProperties' {} a -> s {nextToken = a} :: DescribePatchProperties)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describePatchProperties_maxResults :: Lens.Lens' DescribePatchProperties (Core.Maybe Core.Natural)
describePatchProperties_maxResults = Lens.lens (\DescribePatchProperties' {maxResults} -> maxResults) (\s@DescribePatchProperties' {} a -> s {maxResults = a} :: DescribePatchProperties)

-- | Indicates whether to list patches for the Windows operating system or
-- for Microsoft applications. Not applicable for the Linux or macOS
-- operating systems.
describePatchProperties_patchSet :: Lens.Lens' DescribePatchProperties (Core.Maybe PatchSet)
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
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePatchPropertiesResponse_properties
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePatchProperties_nextToken
          Lens..~ rs
          Lens.^? describePatchPropertiesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribePatchProperties where
  type
    AWSResponse DescribePatchProperties =
      DescribePatchPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchPropertiesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Properties" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePatchProperties

instance Core.NFData DescribePatchProperties

instance Core.ToHeaders DescribePatchProperties where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribePatchProperties" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePatchProperties where
  toJSON DescribePatchProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("PatchSet" Core..=) Core.<$> patchSet,
            Core.Just
              ("OperatingSystem" Core..= operatingSystem),
            Core.Just ("Property" Core..= property)
          ]
      )

instance Core.ToPath DescribePatchProperties where
  toPath = Core.const "/"

instance Core.ToQuery DescribePatchProperties where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePatchPropertiesResponse' smart constructor.
data DescribePatchPropertiesResponse = DescribePatchPropertiesResponse'
  { -- | The token for the next set of items to return. (You use this token in
    -- the next call.)
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the properties for patches matching the filter request
    -- parameters.
    properties :: Core.Maybe [Core.HashMap Core.Text Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribePatchPropertiesResponse
newDescribePatchPropertiesResponse pHttpStatus_ =
  DescribePatchPropertiesResponse'
    { nextToken =
        Core.Nothing,
      properties = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. (You use this token in
-- the next call.)
describePatchPropertiesResponse_nextToken :: Lens.Lens' DescribePatchPropertiesResponse (Core.Maybe Core.Text)
describePatchPropertiesResponse_nextToken = Lens.lens (\DescribePatchPropertiesResponse' {nextToken} -> nextToken) (\s@DescribePatchPropertiesResponse' {} a -> s {nextToken = a} :: DescribePatchPropertiesResponse)

-- | A list of the properties for patches matching the filter request
-- parameters.
describePatchPropertiesResponse_properties :: Lens.Lens' DescribePatchPropertiesResponse (Core.Maybe [Core.HashMap Core.Text Core.Text])
describePatchPropertiesResponse_properties = Lens.lens (\DescribePatchPropertiesResponse' {properties} -> properties) (\s@DescribePatchPropertiesResponse' {} a -> s {properties = a} :: DescribePatchPropertiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePatchPropertiesResponse_httpStatus :: Lens.Lens' DescribePatchPropertiesResponse Core.Int
describePatchPropertiesResponse_httpStatus = Lens.lens (\DescribePatchPropertiesResponse' {httpStatus} -> httpStatus) (\s@DescribePatchPropertiesResponse' {} a -> s {httpStatus = a} :: DescribePatchPropertiesResponse)

instance Core.NFData DescribePatchPropertiesResponse
