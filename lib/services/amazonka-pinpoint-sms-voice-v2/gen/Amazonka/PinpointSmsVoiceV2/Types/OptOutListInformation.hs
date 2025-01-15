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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.OptOutListInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.OptOutListInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information for all OptOutList in an Amazon Web Services account.
--
-- /See:/ 'newOptOutListInformation' smart constructor.
data OptOutListInformation = OptOutListInformation'
  { -- | The Amazon Resource Name (ARN) of the OptOutList.
    optOutListArn :: Prelude.Text,
    -- | The name of the OptOutList.
    optOutListName :: Prelude.Text,
    -- | The time when the OutOutList was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptOutListInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optOutListArn', 'optOutListInformation_optOutListArn' - The Amazon Resource Name (ARN) of the OptOutList.
--
-- 'optOutListName', 'optOutListInformation_optOutListName' - The name of the OptOutList.
--
-- 'createdTimestamp', 'optOutListInformation_createdTimestamp' - The time when the OutOutList was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
newOptOutListInformation ::
  -- | 'optOutListArn'
  Prelude.Text ->
  -- | 'optOutListName'
  Prelude.Text ->
  -- | 'createdTimestamp'
  Prelude.UTCTime ->
  OptOutListInformation
newOptOutListInformation
  pOptOutListArn_
  pOptOutListName_
  pCreatedTimestamp_ =
    OptOutListInformation'
      { optOutListArn =
          pOptOutListArn_,
        optOutListName = pOptOutListName_,
        createdTimestamp =
          Data._Time Lens.# pCreatedTimestamp_
      }

-- | The Amazon Resource Name (ARN) of the OptOutList.
optOutListInformation_optOutListArn :: Lens.Lens' OptOutListInformation Prelude.Text
optOutListInformation_optOutListArn = Lens.lens (\OptOutListInformation' {optOutListArn} -> optOutListArn) (\s@OptOutListInformation' {} a -> s {optOutListArn = a} :: OptOutListInformation)

-- | The name of the OptOutList.
optOutListInformation_optOutListName :: Lens.Lens' OptOutListInformation Prelude.Text
optOutListInformation_optOutListName = Lens.lens (\OptOutListInformation' {optOutListName} -> optOutListName) (\s@OptOutListInformation' {} a -> s {optOutListName = a} :: OptOutListInformation)

-- | The time when the OutOutList was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
optOutListInformation_createdTimestamp :: Lens.Lens' OptOutListInformation Prelude.UTCTime
optOutListInformation_createdTimestamp = Lens.lens (\OptOutListInformation' {createdTimestamp} -> createdTimestamp) (\s@OptOutListInformation' {} a -> s {createdTimestamp = a} :: OptOutListInformation) Prelude.. Data._Time

instance Data.FromJSON OptOutListInformation where
  parseJSON =
    Data.withObject
      "OptOutListInformation"
      ( \x ->
          OptOutListInformation'
            Prelude.<$> (x Data..: "OptOutListArn")
            Prelude.<*> (x Data..: "OptOutListName")
            Prelude.<*> (x Data..: "CreatedTimestamp")
      )

instance Prelude.Hashable OptOutListInformation where
  hashWithSalt _salt OptOutListInformation' {..} =
    _salt
      `Prelude.hashWithSalt` optOutListArn
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData OptOutListInformation where
  rnf OptOutListInformation' {..} =
    Prelude.rnf optOutListArn `Prelude.seq`
      Prelude.rnf optOutListName `Prelude.seq`
        Prelude.rnf createdTimestamp
