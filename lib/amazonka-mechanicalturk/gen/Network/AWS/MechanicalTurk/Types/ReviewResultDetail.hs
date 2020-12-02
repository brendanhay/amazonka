{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewResultDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewResultDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data structure is returned multiple times for each result specified in the Review Policy.
--
--
--
-- /See:/ 'reviewResultDetail' smart constructor.
data ReviewResultDetail = ReviewResultDetail'
  { _rrdValue ::
      !(Maybe Text),
    _rrdActionId :: !(Maybe Text),
    _rrdSubjectType :: !(Maybe Text),
    _rrdKey :: !(Maybe Text),
    _rrdQuestionId :: !(Maybe Text),
    _rrdSubjectId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReviewResultDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrdValue' - The values of Key provided by the review policies you have selected.
--
-- * 'rrdActionId' - A unique identifier of the Review action result.
--
-- * 'rrdSubjectType' - The type of the object from the SubjectId field.
--
-- * 'rrdKey' - Key identifies the particular piece of reviewed information.
--
-- * 'rrdQuestionId' - Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
--
-- * 'rrdSubjectId' - The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
reviewResultDetail ::
  ReviewResultDetail
reviewResultDetail =
  ReviewResultDetail'
    { _rrdValue = Nothing,
      _rrdActionId = Nothing,
      _rrdSubjectType = Nothing,
      _rrdKey = Nothing,
      _rrdQuestionId = Nothing,
      _rrdSubjectId = Nothing
    }

-- | The values of Key provided by the review policies you have selected.
rrdValue :: Lens' ReviewResultDetail (Maybe Text)
rrdValue = lens _rrdValue (\s a -> s {_rrdValue = a})

-- | A unique identifier of the Review action result.
rrdActionId :: Lens' ReviewResultDetail (Maybe Text)
rrdActionId = lens _rrdActionId (\s a -> s {_rrdActionId = a})

-- | The type of the object from the SubjectId field.
rrdSubjectType :: Lens' ReviewResultDetail (Maybe Text)
rrdSubjectType = lens _rrdSubjectType (\s a -> s {_rrdSubjectType = a})

-- | Key identifies the particular piece of reviewed information.
rrdKey :: Lens' ReviewResultDetail (Maybe Text)
rrdKey = lens _rrdKey (\s a -> s {_rrdKey = a})

-- | Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
rrdQuestionId :: Lens' ReviewResultDetail (Maybe Text)
rrdQuestionId = lens _rrdQuestionId (\s a -> s {_rrdQuestionId = a})

-- | The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
rrdSubjectId :: Lens' ReviewResultDetail (Maybe Text)
rrdSubjectId = lens _rrdSubjectId (\s a -> s {_rrdSubjectId = a})

instance FromJSON ReviewResultDetail where
  parseJSON =
    withObject
      "ReviewResultDetail"
      ( \x ->
          ReviewResultDetail'
            <$> (x .:? "Value")
            <*> (x .:? "ActionId")
            <*> (x .:? "SubjectType")
            <*> (x .:? "Key")
            <*> (x .:? "QuestionId")
            <*> (x .:? "SubjectId")
      )

instance Hashable ReviewResultDetail

instance NFData ReviewResultDetail
