{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.KendraConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.KendraConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration information for the AMAZON.KendraSearchIntent intent. When you use this intent, Amazon Lex searches the specified Amazon Kendra index and returns documents from the index that match the user's utterance. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
--
--
--
-- /See:/ 'kendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { _kcQueryFilterString ::
      !(Maybe Text),
    _kcKendraIndex :: !Text,
    _kcRole :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KendraConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kcQueryFilterString' - A query filter that Amazon Lex sends to Amazon Kendra to filter the response from the query. The filter is in the format defined by Amazon Kendra. For more information, see <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries> . You can override this filter string with a new filter string at runtime.
--
-- * 'kcKendraIndex' - The Amazon Resource Name (ARN) of the Amazon Kendra index that you want the AMAZON.KendraSearchIntent intent to search. The index must be in the same account and Region as the Amazon Lex bot. If the Amazon Kendra index does not exist, you get an exception when you call the @PutIntent@ operation.
--
-- * 'kcRole' - The Amazon Resource Name (ARN) of an IAM role that has permission to search the Amazon Kendra index. The role must be in the same account and Region as the Amazon Lex bot. If the role does not exist, you get an exception when you call the @PutIntent@ operation.
kendraConfiguration ::
  -- | 'kcKendraIndex'
  Text ->
  -- | 'kcRole'
  Text ->
  KendraConfiguration
kendraConfiguration pKendraIndex_ pRole_ =
  KendraConfiguration'
    { _kcQueryFilterString = Nothing,
      _kcKendraIndex = pKendraIndex_,
      _kcRole = pRole_
    }

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the response from the query. The filter is in the format defined by Amazon Kendra. For more information, see <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries> . You can override this filter string with a new filter string at runtime.
kcQueryFilterString :: Lens' KendraConfiguration (Maybe Text)
kcQueryFilterString = lens _kcQueryFilterString (\s a -> s {_kcQueryFilterString = a})

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want the AMAZON.KendraSearchIntent intent to search. The index must be in the same account and Region as the Amazon Lex bot. If the Amazon Kendra index does not exist, you get an exception when you call the @PutIntent@ operation.
kcKendraIndex :: Lens' KendraConfiguration Text
kcKendraIndex = lens _kcKendraIndex (\s a -> s {_kcKendraIndex = a})

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to search the Amazon Kendra index. The role must be in the same account and Region as the Amazon Lex bot. If the role does not exist, you get an exception when you call the @PutIntent@ operation.
kcRole :: Lens' KendraConfiguration Text
kcRole = lens _kcRole (\s a -> s {_kcRole = a})

instance FromJSON KendraConfiguration where
  parseJSON =
    withObject
      "KendraConfiguration"
      ( \x ->
          KendraConfiguration'
            <$> (x .:? "queryFilterString")
            <*> (x .: "kendraIndex")
            <*> (x .: "role")
      )

instance Hashable KendraConfiguration

instance NFData KendraConfiguration

instance ToJSON KendraConfiguration where
  toJSON KendraConfiguration' {..} =
    object
      ( catMaybes
          [ ("queryFilterString" .=) <$> _kcQueryFilterString,
            Just ("kendraIndex" .= _kcKendraIndex),
            Just ("role" .= _kcRole)
          ]
      )
