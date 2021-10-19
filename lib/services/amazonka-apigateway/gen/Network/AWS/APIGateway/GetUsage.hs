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
-- Module      : Network.AWS.APIGateway.GetUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the usage data of a usage plan in a specified time interval.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsage
  ( -- * Creating a Request
    GetUsage (..),
    newGetUsage,

    -- * Request Lenses
    getUsage_keyId,
    getUsage_limit,
    getUsage_position,
    getUsage_usagePlanId,
    getUsage_startDate,
    getUsage_endDate,

    -- * Destructuring the Response
    Usage (..),
    newUsage,

    -- * Response Lenses
    usage_usagePlanId,
    usage_endDate,
    usage_items,
    usage_startDate,
    usage_position,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get the usage data of a usage plan in a specified
-- time interval.
--
-- /See:/ 'newGetUsage' smart constructor.
data GetUsage = GetUsage'
  { -- | The Id of the API key associated with the resultant usage data.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | [Required] The Id of the usage plan associated with the usage data.
    usagePlanId :: Prelude.Text,
    -- | [Required] The starting date (e.g., 2016-01-01) of the usage data.
    startDate :: Prelude.Text,
    -- | [Required] The ending date (e.g., 2016-12-31) of the usage data.
    endDate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'getUsage_keyId' - The Id of the API key associated with the resultant usage data.
--
-- 'limit', 'getUsage_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getUsage_position' - The current pagination position in the paged result set.
--
-- 'usagePlanId', 'getUsage_usagePlanId' - [Required] The Id of the usage plan associated with the usage data.
--
-- 'startDate', 'getUsage_startDate' - [Required] The starting date (e.g., 2016-01-01) of the usage data.
--
-- 'endDate', 'getUsage_endDate' - [Required] The ending date (e.g., 2016-12-31) of the usage data.
newGetUsage ::
  -- | 'usagePlanId'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.Text ->
  -- | 'endDate'
  Prelude.Text ->
  GetUsage
newGetUsage pUsagePlanId_ pStartDate_ pEndDate_ =
  GetUsage'
    { keyId = Prelude.Nothing,
      limit = Prelude.Nothing,
      position = Prelude.Nothing,
      usagePlanId = pUsagePlanId_,
      startDate = pStartDate_,
      endDate = pEndDate_
    }

-- | The Id of the API key associated with the resultant usage data.
getUsage_keyId :: Lens.Lens' GetUsage (Prelude.Maybe Prelude.Text)
getUsage_keyId = Lens.lens (\GetUsage' {keyId} -> keyId) (\s@GetUsage' {} a -> s {keyId = a} :: GetUsage)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getUsage_limit :: Lens.Lens' GetUsage (Prelude.Maybe Prelude.Int)
getUsage_limit = Lens.lens (\GetUsage' {limit} -> limit) (\s@GetUsage' {} a -> s {limit = a} :: GetUsage)

-- | The current pagination position in the paged result set.
getUsage_position :: Lens.Lens' GetUsage (Prelude.Maybe Prelude.Text)
getUsage_position = Lens.lens (\GetUsage' {position} -> position) (\s@GetUsage' {} a -> s {position = a} :: GetUsage)

-- | [Required] The Id of the usage plan associated with the usage data.
getUsage_usagePlanId :: Lens.Lens' GetUsage Prelude.Text
getUsage_usagePlanId = Lens.lens (\GetUsage' {usagePlanId} -> usagePlanId) (\s@GetUsage' {} a -> s {usagePlanId = a} :: GetUsage)

-- | [Required] The starting date (e.g., 2016-01-01) of the usage data.
getUsage_startDate :: Lens.Lens' GetUsage Prelude.Text
getUsage_startDate = Lens.lens (\GetUsage' {startDate} -> startDate) (\s@GetUsage' {} a -> s {startDate = a} :: GetUsage)

-- | [Required] The ending date (e.g., 2016-12-31) of the usage data.
getUsage_endDate :: Lens.Lens' GetUsage Prelude.Text
getUsage_endDate = Lens.lens (\GetUsage' {endDate} -> endDate) (\s@GetUsage' {} a -> s {endDate = a} :: GetUsage)

instance Core.AWSPager GetUsage where
  page rq rs
    | Core.stop
        (rs Lens.^? usage_position Prelude.. Lens._Just) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^? usage_items Prelude.. Lens._Just) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getUsage_position
          Lens..~ rs Lens.^? usage_position Prelude.. Lens._Just

instance Core.AWSRequest GetUsage where
  type AWSResponse GetUsage = Usage
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetUsage

instance Prelude.NFData GetUsage

instance Core.ToHeaders GetUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetUsage where
  toPath GetUsage' {..} =
    Prelude.mconcat
      ["/usageplans/", Core.toBS usagePlanId, "/usage"]

instance Core.ToQuery GetUsage where
  toQuery GetUsage' {..} =
    Prelude.mconcat
      [ "keyId" Core.=: keyId,
        "limit" Core.=: limit,
        "position" Core.=: position,
        "startDate" Core.=: startDate,
        "endDate" Core.=: endDate
      ]
