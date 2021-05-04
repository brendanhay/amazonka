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
-- Module      : Network.AWS.LexModels.Types.KendraConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.KendraConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for the AMAZON.KendraSearchIntent
-- intent. When you use this intent, Amazon Lex searches the specified
-- Amazon Kendra index and returns documents from the index that match the
-- user\'s utterance. For more information, see
-- <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent>.
--
-- /See:/ 'newKendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { -- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
    -- response from the query. The filter is in the format defined by Amazon
    -- Kendra. For more information, see
    -- <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
    --
    -- You can override this filter string with a new filter string at runtime.
    queryFilterString :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
    -- the AMAZON.KendraSearchIntent intent to search. The index must be in the
    -- same account and Region as the Amazon Lex bot. If the Amazon Kendra
    -- index does not exist, you get an exception when you call the @PutIntent@
    -- operation.
    kendraIndex :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
    -- search the Amazon Kendra index. The role must be in the same account and
    -- Region as the Amazon Lex bot. If the role does not exist, you get an
    -- exception when you call the @PutIntent@ operation.
    role' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KendraConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryFilterString', 'kendraConfiguration_queryFilterString' - A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from the query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
--
-- You can override this filter string with a new filter string at runtime.
--
-- 'kendraIndex', 'kendraConfiguration_kendraIndex' - The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot. If the Amazon Kendra
-- index does not exist, you get an exception when you call the @PutIntent@
-- operation.
--
-- 'role'', 'kendraConfiguration_role' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- search the Amazon Kendra index. The role must be in the same account and
-- Region as the Amazon Lex bot. If the role does not exist, you get an
-- exception when you call the @PutIntent@ operation.
newKendraConfiguration ::
  -- | 'kendraIndex'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  KendraConfiguration
newKendraConfiguration pKendraIndex_ pRole_ =
  KendraConfiguration'
    { queryFilterString =
        Prelude.Nothing,
      kendraIndex = pKendraIndex_,
      role' = pRole_
    }

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from the query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
--
-- You can override this filter string with a new filter string at runtime.
kendraConfiguration_queryFilterString :: Lens.Lens' KendraConfiguration (Prelude.Maybe Prelude.Text)
kendraConfiguration_queryFilterString = Lens.lens (\KendraConfiguration' {queryFilterString} -> queryFilterString) (\s@KendraConfiguration' {} a -> s {queryFilterString = a} :: KendraConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot. If the Amazon Kendra
-- index does not exist, you get an exception when you call the @PutIntent@
-- operation.
kendraConfiguration_kendraIndex :: Lens.Lens' KendraConfiguration Prelude.Text
kendraConfiguration_kendraIndex = Lens.lens (\KendraConfiguration' {kendraIndex} -> kendraIndex) (\s@KendraConfiguration' {} a -> s {kendraIndex = a} :: KendraConfiguration)

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- search the Amazon Kendra index. The role must be in the same account and
-- Region as the Amazon Lex bot. If the role does not exist, you get an
-- exception when you call the @PutIntent@ operation.
kendraConfiguration_role :: Lens.Lens' KendraConfiguration Prelude.Text
kendraConfiguration_role = Lens.lens (\KendraConfiguration' {role'} -> role') (\s@KendraConfiguration' {} a -> s {role' = a} :: KendraConfiguration)

instance Prelude.FromJSON KendraConfiguration where
  parseJSON =
    Prelude.withObject
      "KendraConfiguration"
      ( \x ->
          KendraConfiguration'
            Prelude.<$> (x Prelude..:? "queryFilterString")
            Prelude.<*> (x Prelude..: "kendraIndex")
            Prelude.<*> (x Prelude..: "role")
      )

instance Prelude.Hashable KendraConfiguration

instance Prelude.NFData KendraConfiguration

instance Prelude.ToJSON KendraConfiguration where
  toJSON KendraConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("queryFilterString" Prelude..=)
              Prelude.<$> queryFilterString,
            Prelude.Just ("kendraIndex" Prelude..= kendraIndex),
            Prelude.Just ("role" Prelude..= role')
          ]
      )
