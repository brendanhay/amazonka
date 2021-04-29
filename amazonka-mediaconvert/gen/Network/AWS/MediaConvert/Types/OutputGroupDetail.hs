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
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupDetail where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.OutputDetail
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about the output groups specified in the job settings.
--
-- /See:/ 'newOutputGroupDetail' smart constructor.
data OutputGroupDetail = OutputGroupDetail'
  { -- | Details about the output
    outputDetails :: Prelude.Maybe [OutputDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputGroupDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDetails', 'outputGroupDetail_outputDetails' - Details about the output
newOutputGroupDetail ::
  OutputGroupDetail
newOutputGroupDetail =
  OutputGroupDetail' {outputDetails = Prelude.Nothing}

-- | Details about the output
outputGroupDetail_outputDetails :: Lens.Lens' OutputGroupDetail (Prelude.Maybe [OutputDetail])
outputGroupDetail_outputDetails = Lens.lens (\OutputGroupDetail' {outputDetails} -> outputDetails) (\s@OutputGroupDetail' {} a -> s {outputDetails = a} :: OutputGroupDetail) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON OutputGroupDetail where
  parseJSON =
    Prelude.withObject
      "OutputGroupDetail"
      ( \x ->
          OutputGroupDetail'
            Prelude.<$> ( x Prelude..:? "outputDetails"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OutputGroupDetail

instance Prelude.NFData OutputGroupDetail
