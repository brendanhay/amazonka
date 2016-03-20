{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.Product where

import           Network.AWS.CloudWatchEvents.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Contains information about the event to be used in PutEvents.
--
-- /See:/ 'putEventsRequestEntry' smart constructor.
data PutEventsRequestEntry = PutEventsRequestEntry'
    { _pereTime       :: !(Maybe POSIX)
    , _pereDetailType :: !(Maybe Text)
    , _pereResources  :: !(Maybe [Text])
    , _pereSource     :: !(Maybe Text)
    , _pereDetail     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutEventsRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pereTime'
--
-- * 'pereDetailType'
--
-- * 'pereResources'
--
-- * 'pereSource'
--
-- * 'pereDetail'
putEventsRequestEntry
    :: PutEventsRequestEntry
putEventsRequestEntry =
    PutEventsRequestEntry'
    { _pereTime = Nothing
    , _pereDetailType = Nothing
    , _pereResources = Nothing
    , _pereSource = Nothing
    , _pereDetail = Nothing
    }

-- | Timestamp of event, per
-- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no timestamp is
-- provided, the timestamp of the < PutEvents> call will be used.
pereTime :: Lens' PutEventsRequestEntry (Maybe UTCTime)
pereTime = lens _pereTime (\ s a -> s{_pereTime = a}) . mapping _Time;

-- | Free-form string used to decide what fields to expect in the event
-- detail.
pereDetailType :: Lens' PutEventsRequestEntry (Maybe Text)
pereDetailType = lens _pereDetailType (\ s a -> s{_pereDetailType = a});

-- | AWS resources, identified by Amazon Resource Name (ARN), which the event
-- primarily concerns. Any number, including zero, may be present.
pereResources :: Lens' PutEventsRequestEntry [Text]
pereResources = lens _pereResources (\ s a -> s{_pereResources = a}) . _Default . _Coerce;

-- | The source of the event.
pereSource :: Lens' PutEventsRequestEntry (Maybe Text)
pereSource = lens _pereSource (\ s a -> s{_pereSource = a});

-- | In the JSON sense, an object containing fields, which may also contain
-- nested sub-objects. No constraints are imposed on its contents.
pereDetail :: Lens' PutEventsRequestEntry (Maybe Text)
pereDetail = lens _pereDetail (\ s a -> s{_pereDetail = a});

instance Hashable PutEventsRequestEntry

instance ToJSON PutEventsRequestEntry where
        toJSON PutEventsRequestEntry'{..}
          = object
              (catMaybes
                 [("Time" .=) <$> _pereTime,
                  ("DetailType" .=) <$> _pereDetailType,
                  ("Resources" .=) <$> _pereResources,
                  ("Source" .=) <$> _pereSource,
                  ("Detail" .=) <$> _pereDetail])

-- | A PutEventsResult contains a list of PutEventsResultEntry.
--
-- /See:/ 'putEventsResultEntry' smart constructor.
data PutEventsResultEntry = PutEventsResultEntry'
    { _pereErrorCode    :: !(Maybe Text)
    , _pereErrorMessage :: !(Maybe Text)
    , _pereEventId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutEventsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pereErrorCode'
--
-- * 'pereErrorMessage'
--
-- * 'pereEventId'
putEventsResultEntry
    :: PutEventsResultEntry
putEventsResultEntry =
    PutEventsResultEntry'
    { _pereErrorCode = Nothing
    , _pereErrorMessage = Nothing
    , _pereEventId = Nothing
    }

-- | The error code representing why the event submission failed on this
-- entry.
pereErrorCode :: Lens' PutEventsResultEntry (Maybe Text)
pereErrorCode = lens _pereErrorCode (\ s a -> s{_pereErrorCode = a});

-- | The error message explaining why the event submission failed on this
-- entry.
pereErrorMessage :: Lens' PutEventsResultEntry (Maybe Text)
pereErrorMessage = lens _pereErrorMessage (\ s a -> s{_pereErrorMessage = a});

-- | The ID of the event submitted to Amazon CloudWatch Events.
pereEventId :: Lens' PutEventsResultEntry (Maybe Text)
pereEventId = lens _pereEventId (\ s a -> s{_pereEventId = a});

instance FromJSON PutEventsResultEntry where
        parseJSON
          = withObject "PutEventsResultEntry"
              (\ x ->
                 PutEventsResultEntry' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage") <*>
                     (x .:? "EventId"))

instance Hashable PutEventsResultEntry

-- | A PutTargetsResult contains a list of PutTargetsResultEntry.
--
-- /See:/ 'putTargetsResultEntry' smart constructor.
data PutTargetsResultEntry = PutTargetsResultEntry'
    { _ptreTargetId     :: !(Maybe Text)
    , _ptreErrorCode    :: !(Maybe Text)
    , _ptreErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutTargetsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptreTargetId'
--
-- * 'ptreErrorCode'
--
-- * 'ptreErrorMessage'
putTargetsResultEntry
    :: PutTargetsResultEntry
putTargetsResultEntry =
    PutTargetsResultEntry'
    { _ptreTargetId = Nothing
    , _ptreErrorCode = Nothing
    , _ptreErrorMessage = Nothing
    }

-- | The ID of the target submitted to Amazon CloudWatch Events.
ptreTargetId :: Lens' PutTargetsResultEntry (Maybe Text)
ptreTargetId = lens _ptreTargetId (\ s a -> s{_ptreTargetId = a});

-- | The error code representing why the target submission failed on this
-- entry.
ptreErrorCode :: Lens' PutTargetsResultEntry (Maybe Text)
ptreErrorCode = lens _ptreErrorCode (\ s a -> s{_ptreErrorCode = a});

-- | The error message explaining why the target submission failed on this
-- entry.
ptreErrorMessage :: Lens' PutTargetsResultEntry (Maybe Text)
ptreErrorMessage = lens _ptreErrorMessage (\ s a -> s{_ptreErrorMessage = a});

instance FromJSON PutTargetsResultEntry where
        parseJSON
          = withObject "PutTargetsResultEntry"
              (\ x ->
                 PutTargetsResultEntry' <$>
                   (x .:? "TargetId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable PutTargetsResultEntry

-- | The ID of the target requested to be removed from the rule by Amazon
-- CloudWatch Events.
--
-- /See:/ 'removeTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
    { _rtreTargetId     :: !(Maybe Text)
    , _rtreErrorCode    :: !(Maybe Text)
    , _rtreErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveTargetsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtreTargetId'
--
-- * 'rtreErrorCode'
--
-- * 'rtreErrorMessage'
removeTargetsResultEntry
    :: RemoveTargetsResultEntry
removeTargetsResultEntry =
    RemoveTargetsResultEntry'
    { _rtreTargetId = Nothing
    , _rtreErrorCode = Nothing
    , _rtreErrorMessage = Nothing
    }

-- | The ID of the target requested to be removed by Amazon CloudWatch
-- Events.
rtreTargetId :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreTargetId = lens _rtreTargetId (\ s a -> s{_rtreTargetId = a});

-- | The error code representing why the target removal failed on this entry.
rtreErrorCode :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreErrorCode = lens _rtreErrorCode (\ s a -> s{_rtreErrorCode = a});

-- | The error message explaining why the target removal failed on this
-- entry.
rtreErrorMessage :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreErrorMessage = lens _rtreErrorMessage (\ s a -> s{_rtreErrorMessage = a});

instance FromJSON RemoveTargetsResultEntry where
        parseJSON
          = withObject "RemoveTargetsResultEntry"
              (\ x ->
                 RemoveTargetsResultEntry' <$>
                   (x .:? "TargetId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable RemoveTargetsResultEntry

-- | Contains information about a rule in Amazon CloudWatch Events. A
-- ListRulesResult contains a list of Rules.
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
    { _rEventPattern       :: !(Maybe Text)
    , _rState              :: !(Maybe RuleState)
    , _rARN                :: !(Maybe Text)
    , _rScheduleExpression :: !(Maybe Text)
    , _rName               :: !(Maybe Text)
    , _rDescription        :: !(Maybe Text)
    , _rRoleARN            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEventPattern'
--
-- * 'rState'
--
-- * 'rARN'
--
-- * 'rScheduleExpression'
--
-- * 'rName'
--
-- * 'rDescription'
--
-- * 'rRoleARN'
rule
    :: Rule
rule =
    Rule'
    { _rEventPattern = Nothing
    , _rState = Nothing
    , _rARN = Nothing
    , _rScheduleExpression = Nothing
    , _rName = Nothing
    , _rDescription = Nothing
    , _rRoleARN = Nothing
    }

-- | The event pattern of the rule.
rEventPattern :: Lens' Rule (Maybe Text)
rEventPattern = lens _rEventPattern (\ s a -> s{_rEventPattern = a});

-- | The rule\'s state.
rState :: Lens' Rule (Maybe RuleState)
rState = lens _rState (\ s a -> s{_rState = a});

-- | The Amazon Resource Name (ARN) of the rule.
rARN :: Lens' Rule (Maybe Text)
rARN = lens _rARN (\ s a -> s{_rARN = a});

-- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\".
rScheduleExpression :: Lens' Rule (Maybe Text)
rScheduleExpression = lens _rScheduleExpression (\ s a -> s{_rScheduleExpression = a});

-- | The rule\'s name.
rName :: Lens' Rule (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | The description of the rule.
rDescription :: Lens' Rule (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a});

-- | The Amazon Resource Name (ARN) associated with the role that is used for
-- target invocation.
rRoleARN :: Lens' Rule (Maybe Text)
rRoleARN = lens _rRoleARN (\ s a -> s{_rRoleARN = a});

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "EventPattern") <*> (x .:? "State") <*>
                     (x .:? "Arn")
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "Description")
                     <*> (x .:? "RoleArn"))

instance Hashable Rule

-- | Targets are the resources that can be invoked when a rule is triggered.
-- For example, AWS Lambda functions, Amazon Kinesis streams, and built-in
-- targets.
--
-- __Input__ and __InputPath__ are mutually-exclusive and optional
-- parameters of a target. When a rule is triggered due to a matched event,
-- if for a target:
--
-- -   Neither __Input__ nor __InputPath__ is specified, then the entire
--     event is passed to the target in JSON form.
-- -   __InputPath__ is specified in the form of JSONPath (e.g.
--     __$.detail__), then only the part of the event specified in the path
--     is passed to the target (e.g. only the detail part of the event is
--     passed).
-- -   __Input__ is specified in the form of a valid JSON, then the matched
--     event is overridden with this constant.
--
-- /See:/ 'target' smart constructor.
data Target = Target'
    { _tInput     :: !(Maybe Text)
    , _tInputPath :: !(Maybe Text)
    , _tId        :: !Text
    , _tARN       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tInput'
--
-- * 'tInputPath'
--
-- * 'tId'
--
-- * 'tARN'
target
    :: Text -- ^ 'tId'
    -> Text -- ^ 'tARN'
    -> Target
target pId_ pARN_ =
    Target'
    { _tInput = Nothing
    , _tInputPath = Nothing
    , _tId = pId_
    , _tARN = pARN_
    }

-- | Valid JSON text passed to the target. For more information about JSON
-- text, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
tInput :: Lens' Target (Maybe Text)
tInput = lens _tInput (\ s a -> s{_tInput = a});

-- | The value of the JSONPath that is used for extracting part of the
-- matched event when passing it to the target. For more information about
-- JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath>.
tInputPath :: Lens' Target (Maybe Text)
tInputPath = lens _tInputPath (\ s a -> s{_tInputPath = a});

-- | The unique target assignment ID.
tId :: Lens' Target Text
tId = lens _tId (\ s a -> s{_tId = a});

-- | The Amazon Resource Name (ARN) associated of the target.
tARN :: Lens' Target Text
tARN = lens _tARN (\ s a -> s{_tARN = a});

instance FromJSON Target where
        parseJSON
          = withObject "Target"
              (\ x ->
                 Target' <$>
                   (x .:? "Input") <*> (x .:? "InputPath") <*>
                     (x .: "Id")
                     <*> (x .: "Arn"))

instance Hashable Target

instance ToJSON Target where
        toJSON Target'{..}
          = object
              (catMaybes
                 [("Input" .=) <$> _tInput,
                  ("InputPath" .=) <$> _tInputPath,
                  Just ("Id" .= _tId), Just ("Arn" .= _tARN)])
