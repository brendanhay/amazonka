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
-- Module      : Amazonka.Personalize.Types.CampaignConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.CampaignConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration details of a campaign.
--
-- /See:/ 'newCampaignConfig' smart constructor.
data CampaignConfig = CampaignConfig'
  { -- | Specifies the exploration configuration hyperparameters, including
    -- @explorationWeight@ and @explorationItemAgeCutOff@, you want to use to
    -- configure the amount of item exploration Amazon Personalize uses when
    -- recommending items. Provide @itemExplorationConfig@ data only if your
    -- solution uses the
    -- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>
    -- recipe.
    itemExplorationConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemExplorationConfig', 'campaignConfig_itemExplorationConfig' - Specifies the exploration configuration hyperparameters, including
-- @explorationWeight@ and @explorationItemAgeCutOff@, you want to use to
-- configure the amount of item exploration Amazon Personalize uses when
-- recommending items. Provide @itemExplorationConfig@ data only if your
-- solution uses the
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>
-- recipe.
newCampaignConfig ::
  CampaignConfig
newCampaignConfig =
  CampaignConfig'
    { itemExplorationConfig =
        Prelude.Nothing
    }

-- | Specifies the exploration configuration hyperparameters, including
-- @explorationWeight@ and @explorationItemAgeCutOff@, you want to use to
-- configure the amount of item exploration Amazon Personalize uses when
-- recommending items. Provide @itemExplorationConfig@ data only if your
-- solution uses the
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>
-- recipe.
campaignConfig_itemExplorationConfig :: Lens.Lens' CampaignConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
campaignConfig_itemExplorationConfig = Lens.lens (\CampaignConfig' {itemExplorationConfig} -> itemExplorationConfig) (\s@CampaignConfig' {} a -> s {itemExplorationConfig = a} :: CampaignConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CampaignConfig where
  parseJSON =
    Data.withObject
      "CampaignConfig"
      ( \x ->
          CampaignConfig'
            Prelude.<$> ( x
                            Data..:? "itemExplorationConfig"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CampaignConfig where
  hashWithSalt _salt CampaignConfig' {..} =
    _salt `Prelude.hashWithSalt` itemExplorationConfig

instance Prelude.NFData CampaignConfig where
  rnf CampaignConfig' {..} =
    Prelude.rnf itemExplorationConfig

instance Data.ToJSON CampaignConfig where
  toJSON CampaignConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("itemExplorationConfig" Data..=)
              Prelude.<$> itemExplorationConfig
          ]
      )
