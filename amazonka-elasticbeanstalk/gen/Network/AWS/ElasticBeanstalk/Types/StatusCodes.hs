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
-- Module      : Network.AWS.ElasticBeanstalk.Types.StatusCodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.StatusCodes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html Status Code Definitions>.
--
-- /See:/ 'newStatusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { -- | The percentage of requests over the last 10 seconds that resulted in a
    -- 3xx (300, 301, etc.) status code.
    status3xx :: Prelude.Maybe Prelude.Int,
    -- | The percentage of requests over the last 10 seconds that resulted in a
    -- 5xx (500, 501, etc.) status code.
    status5xx :: Prelude.Maybe Prelude.Int,
    -- | The percentage of requests over the last 10 seconds that resulted in a
    -- 2xx (200, 201, etc.) status code.
    status2xx :: Prelude.Maybe Prelude.Int,
    -- | The percentage of requests over the last 10 seconds that resulted in a
    -- 4xx (400, 401, etc.) status code.
    status4xx :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StatusCodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status3xx', 'statusCodes_status3xx' - The percentage of requests over the last 10 seconds that resulted in a
-- 3xx (300, 301, etc.) status code.
--
-- 'status5xx', 'statusCodes_status5xx' - The percentage of requests over the last 10 seconds that resulted in a
-- 5xx (500, 501, etc.) status code.
--
-- 'status2xx', 'statusCodes_status2xx' - The percentage of requests over the last 10 seconds that resulted in a
-- 2xx (200, 201, etc.) status code.
--
-- 'status4xx', 'statusCodes_status4xx' - The percentage of requests over the last 10 seconds that resulted in a
-- 4xx (400, 401, etc.) status code.
newStatusCodes ::
  StatusCodes
newStatusCodes =
  StatusCodes'
    { status3xx = Prelude.Nothing,
      status5xx = Prelude.Nothing,
      status2xx = Prelude.Nothing,
      status4xx = Prelude.Nothing
    }

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 3xx (300, 301, etc.) status code.
statusCodes_status3xx :: Lens.Lens' StatusCodes (Prelude.Maybe Prelude.Int)
statusCodes_status3xx = Lens.lens (\StatusCodes' {status3xx} -> status3xx) (\s@StatusCodes' {} a -> s {status3xx = a} :: StatusCodes)

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 5xx (500, 501, etc.) status code.
statusCodes_status5xx :: Lens.Lens' StatusCodes (Prelude.Maybe Prelude.Int)
statusCodes_status5xx = Lens.lens (\StatusCodes' {status5xx} -> status5xx) (\s@StatusCodes' {} a -> s {status5xx = a} :: StatusCodes)

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 2xx (200, 201, etc.) status code.
statusCodes_status2xx :: Lens.Lens' StatusCodes (Prelude.Maybe Prelude.Int)
statusCodes_status2xx = Lens.lens (\StatusCodes' {status2xx} -> status2xx) (\s@StatusCodes' {} a -> s {status2xx = a} :: StatusCodes)

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 4xx (400, 401, etc.) status code.
statusCodes_status4xx :: Lens.Lens' StatusCodes (Prelude.Maybe Prelude.Int)
statusCodes_status4xx = Lens.lens (\StatusCodes' {status4xx} -> status4xx) (\s@StatusCodes' {} a -> s {status4xx = a} :: StatusCodes)

instance Prelude.FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      Prelude.<$> (x Prelude..@? "Status3xx")
      Prelude.<*> (x Prelude..@? "Status5xx")
      Prelude.<*> (x Prelude..@? "Status2xx")
      Prelude.<*> (x Prelude..@? "Status4xx")

instance Prelude.Hashable StatusCodes

instance Prelude.NFData StatusCodes
