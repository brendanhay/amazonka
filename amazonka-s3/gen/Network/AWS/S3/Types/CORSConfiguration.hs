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
-- Module      : Network.AWS.S3.Types.CORSConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CORSConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CORSRule

-- | Describes the cross-origin access configuration for objects in an Amazon
-- S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- /See:/ 'newCORSConfiguration' smart constructor.
data CORSConfiguration = CORSConfiguration'
  { -- | A set of origins and methods (cross-origin access that you want to
    -- allow). You can add up to 100 rules to the configuration.
    cORSRules :: [CORSRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CORSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cORSRules', 'cORSConfiguration_cORSRules' - A set of origins and methods (cross-origin access that you want to
-- allow). You can add up to 100 rules to the configuration.
newCORSConfiguration ::
  CORSConfiguration
newCORSConfiguration =
  CORSConfiguration' {cORSRules = Prelude.mempty}

-- | A set of origins and methods (cross-origin access that you want to
-- allow). You can add up to 100 rules to the configuration.
cORSConfiguration_cORSRules :: Lens.Lens' CORSConfiguration [CORSRule]
cORSConfiguration_cORSRules = Lens.lens (\CORSConfiguration' {cORSRules} -> cORSRules) (\s@CORSConfiguration' {} a -> s {cORSRules = a} :: CORSConfiguration) Prelude.. Prelude._Coerce

instance Prelude.Hashable CORSConfiguration

instance Prelude.NFData CORSConfiguration

instance Prelude.ToXML CORSConfiguration where
  toXML CORSConfiguration' {..} =
    Prelude.mconcat
      [Prelude.toXMLList "CORSRule" cORSRules]
