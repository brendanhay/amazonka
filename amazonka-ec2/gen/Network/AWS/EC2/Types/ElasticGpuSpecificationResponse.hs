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
-- Module      : Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuSpecificationResponse where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an elastic GPU.
--
-- /See:/ 'newElasticGpuSpecificationResponse' smart constructor.
data ElasticGpuSpecificationResponse = ElasticGpuSpecificationResponse'
  { -- | The elastic GPU type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ElasticGpuSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'elasticGpuSpecificationResponse_type' - The elastic GPU type.
newElasticGpuSpecificationResponse ::
  ElasticGpuSpecificationResponse
newElasticGpuSpecificationResponse =
  ElasticGpuSpecificationResponse'
    { type' =
        Prelude.Nothing
    }

-- | The elastic GPU type.
elasticGpuSpecificationResponse_type :: Lens.Lens' ElasticGpuSpecificationResponse (Prelude.Maybe Prelude.Text)
elasticGpuSpecificationResponse_type = Lens.lens (\ElasticGpuSpecificationResponse' {type'} -> type') (\s@ElasticGpuSpecificationResponse' {} a -> s {type' = a} :: ElasticGpuSpecificationResponse)

instance
  Prelude.FromXML
    ElasticGpuSpecificationResponse
  where
  parseXML x =
    ElasticGpuSpecificationResponse'
      Prelude.<$> (x Prelude..@? "type")

instance
  Prelude.Hashable
    ElasticGpuSpecificationResponse

instance
  Prelude.NFData
    ElasticGpuSpecificationResponse
