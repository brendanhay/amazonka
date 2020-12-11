-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.KendraConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.KendraConfiguration
  ( KendraConfiguration (..),

    -- * Smart constructor
    mkKendraConfiguration,

    -- * Lenses
    kcQueryFilterString,
    kcKendraIndex,
    kcRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration information for the AMAZON.KendraSearchIntent intent. When you use this intent, Amazon Lex searches the specified Amazon Kendra index and returns documents from the index that match the user's utterance. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
--
-- /See:/ 'mkKendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { queryFilterString ::
      Lude.Maybe Lude.Text,
    kendraIndex :: Lude.Text,
    role' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KendraConfiguration' with the minimum fields required to make a request.
--
-- * 'kendraIndex' - The Amazon Resource Name (ARN) of the Amazon Kendra index that you want the AMAZON.KendraSearchIntent intent to search. The index must be in the same account and Region as the Amazon Lex bot. If the Amazon Kendra index does not exist, you get an exception when you call the @PutIntent@ operation.
-- * 'queryFilterString' - A query filter that Amazon Lex sends to Amazon Kendra to filter the response from the query. The filter is in the format defined by Amazon Kendra. For more information, see <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries> .
--
-- You can override this filter string with a new filter string at runtime.
-- * 'role'' - The Amazon Resource Name (ARN) of an IAM role that has permission to search the Amazon Kendra index. The role must be in the same account and Region as the Amazon Lex bot. If the role does not exist, you get an exception when you call the @PutIntent@ operation.
mkKendraConfiguration ::
  -- | 'kendraIndex'
  Lude.Text ->
  -- | 'role''
  Lude.Text ->
  KendraConfiguration
mkKendraConfiguration pKendraIndex_ pRole_ =
  KendraConfiguration'
    { queryFilterString = Lude.Nothing,
      kendraIndex = pKendraIndex_,
      role' = pRole_
    }

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the response from the query. The filter is in the format defined by Amazon Kendra. For more information, see <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries> .
--
-- You can override this filter string with a new filter string at runtime.
--
-- /Note:/ Consider using 'queryFilterString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcQueryFilterString :: Lens.Lens' KendraConfiguration (Lude.Maybe Lude.Text)
kcQueryFilterString = Lens.lens (queryFilterString :: KendraConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {queryFilterString = a} :: KendraConfiguration)
{-# DEPRECATED kcQueryFilterString "Use generic-lens or generic-optics with 'queryFilterString' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want the AMAZON.KendraSearchIntent intent to search. The index must be in the same account and Region as the Amazon Lex bot. If the Amazon Kendra index does not exist, you get an exception when you call the @PutIntent@ operation.
--
-- /Note:/ Consider using 'kendraIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcKendraIndex :: Lens.Lens' KendraConfiguration Lude.Text
kcKendraIndex = Lens.lens (kendraIndex :: KendraConfiguration -> Lude.Text) (\s a -> s {kendraIndex = a} :: KendraConfiguration)
{-# DEPRECATED kcKendraIndex "Use generic-lens or generic-optics with 'kendraIndex' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to search the Amazon Kendra index. The role must be in the same account and Region as the Amazon Lex bot. If the role does not exist, you get an exception when you call the @PutIntent@ operation.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcRole :: Lens.Lens' KendraConfiguration Lude.Text
kcRole = Lens.lens (role' :: KendraConfiguration -> Lude.Text) (\s a -> s {role' = a} :: KendraConfiguration)
{-# DEPRECATED kcRole "Use generic-lens or generic-optics with 'role'' instead." #-}

instance Lude.FromJSON KendraConfiguration where
  parseJSON =
    Lude.withObject
      "KendraConfiguration"
      ( \x ->
          KendraConfiguration'
            Lude.<$> (x Lude..:? "queryFilterString")
            Lude.<*> (x Lude..: "kendraIndex")
            Lude.<*> (x Lude..: "role")
      )

instance Lude.ToJSON KendraConfiguration where
  toJSON KendraConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryFilterString" Lude..=) Lude.<$> queryFilterString,
            Lude.Just ("kendraIndex" Lude..= kendraIndex),
            Lude.Just ("role" Lude..= role')
          ]
      )
